{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Function
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

onCons :: (a -> [a] -> [b]) -> [a] -> [b]
onCons _  []    = []
onCons f (x:xs) = f x xs
{-# INLINE onCons #-}

onHead :: (a -> a) -> [a] -> [a]
onHead _  []    = []
onHead f (x:xs) = f x : xs
{-# INLINE onHead #-}

continuousBy :: (a -> Bool) -> [a] -> [[a]]
continuousBy p = go where
  go xs = [pxs | not $ null pxs] ++ onCons (\x -> onHead (x:) . go) sxs where
    (pxs, sxs) = break p xs
{-# INLINE continuousBy #-}

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = go xs where
  go xs' = pxs : onCons (const go) sxs where
    (pxs, sxs) = break p xs'
{-# INLINE splitBy #-}

gfoldr1 :: (a -> b -> b) -> (a -> b) -> [a] -> b
gfoldr1 g f = go where
  go  []    = error "gfoldr1: empty list"
  go  [x]   = f x
  go (x:xs) = g x (go xs)
{-# INLINE gfoldr1 #-}

breakR :: (a -> Bool) -> [a] -> ([a], [a])
breakR p xs = (reverse prefixR, x : reverse suffixR) where
  (suffixR, x : prefixR) = break p $ reverse xs
{-# INLINE breakR #-}

data Coin a = Heads a | Tails a
            deriving (Functor)

denom :: Coin a -> a
denom (Heads x) = x
denom (Tails x) = x
{-# INLINE denom #-}

toTails :: Coin a -> Coin a
toTails = Tails . denom
{-# INLINE toTails #-}

fromHeads :: Coin a -> Maybe a
fromHeads (Heads x) = Just x
fromHeads (Tails _) = Nothing
{-# INLINE fromHeads #-}

data CppPrim
  = CppVoid
  | CppBool
  | CppInt
  | CppFloat
  | CppDouble

data CppModif
  = CppOpt
  | CppPtr
  | CppRef
  | CppConst
  | CppVirtual
  deriving (Eq, Ord)

data CppType a
  = CppPure  a
  | CppPrim  CppPrim  (CppType a)
  | CppClass String   (CppType a)
  | CppModif CppModif (CppType a)

type TypedName = CppType String

data FunDecl = FunDecl
  { funResult  :: TypedName
  , funParams  :: [TypedName]
  , funIsConst :: Bool
  }

getName :: TypedName -> String
getName (CppPure  name)   = name
getName (CppPrim  _ rest) = getName rest
getName (CppClass _ rest) = getName rest
getName (CppModif _ rest) = getName rest

sortModifs :: CppType a -> CppType a
sortModifs = go $ \ms a -> foldr CppModif a $ sort ms where
  go k (CppPure  x)   = k [] (CppPure x)
  go k (CppPrim  s t) = go (\ms -> k ms . CppPrim  s) t
  go k (CppClass s t) = go (\ms -> k ms . CppClass s) t
  go k (CppModif m t) = go (k . (m :)) t

normType :: CppType a -> CppType a
normType = go . sortModifs where
  go (CppModif CppRef (CppModif CppConst (CppClass s t))) = CppClass s  t
  go (CppModif m       t                                ) = CppModif m (go t)
  go  t                                                   = t

parsePrim :: String -> Maybe CppPrim
parsePrim "void"   = Just CppVoid
parsePrim "bool"   = Just CppBool
parsePrim "int"    = Just CppInt
parsePrim "float"  = Just CppFloat
parsePrim "double" = Just CppDouble
parsePrim "qreal"  = Just CppDouble
parsePrim _        = Nothing

parseModif :: String -> Maybe CppModif
parseModif "&"       = Just CppRef
parseModif "*"       = Just CppPtr
parseModif "const"   = Just CppConst
parseModif "virtual" = Just CppVirtual
parseModif _         = Nothing

growType :: String -> CppType a -> CppType a
growType s
  | null s                     = id
  | Just prim  <- parsePrim  s = CppPrim  prim
  | Just modif <- parseModif s = CppModif modif
  | otherwise                  = CppClass s

parseTypedName :: String -> TypedName
parseTypedName s = normType . gfoldr1 growType acc $ words noopt where
  (noopt, eqopt) = break (== '=') s
  acc = if null eqopt then CppPure else CppModif CppOpt . CppPure

parseFunDecl :: String -> FunDecl
parseFunDecl str = FunDecl result params isConst where
  (preresult, '(':rest ) = break  (== '(') str
  (comParams, ')':modif) = breakR (== ')') rest
  result  = parseTypedName preresult
  params  = map parseTypedName $ splitBy (== ',') comParams
  isConst = filter (not . isSpace) modif == "const"

toQtahPrim :: CppPrim -> String
toQtahPrim CppVoid   = "voidT"
toQtahPrim CppBool   = "boolT"
toQtahPrim CppInt    = "intT"
toQtahPrim CppFloat  = "floatT"
toQtahPrim CppDouble = "doubleT"

toQtahModif :: CppModif -> Maybe String
toQtahModif CppOpt     = Nothing
toQtahModif CppPtr     = Just "ptrT"
toQtahModif CppRef     = Just "refT"
toQtahModif CppConst   = Just "constT"
toQtahModif CppVirtual = Just ""

-- optional => Tails.
toQtahParam :: TypedName -> Coin String
toQtahParam (CppPure  name)       = Heads name
toQtahParam (CppPrim  prim  _   ) = Heads $ toQtahPrim prim
toQtahParam (CppClass cl    _   ) = Heads $ "objT c_" ++ cl
toQtahParam (CppModif modif rest) = case toQtahModif modif of
  Nothing        -> toTails $ toQtahParam rest
  Just qtahModif -> (\qtahRest -> qtahModif ++ " $ " ++ qtahRest) <$> toQtahParam rest

goSurroundParams :: (String -> String) -> TypedName -> String -> String
goSurroundParams alterName (CppPure  name)       pars =
  concat ["\"", alterName name, "\" ", pars]
goSurroundParams alterName (CppPrim  prim  rest) pars =
  goSurroundParams alterName rest (pars ++ " " ++ toQtahPrim prim)
goSurroundParams alterName (CppClass cl    rest) pars =
  goSurroundParams alterName rest (pars ++ " $ objT c_" ++ cl)
goSurroundParams alterName (CppModif modif rest) pars =
  goSurroundParams alterName rest $ case modif of
    CppOpt     -> pars
    CppPtr     -> pars ++ " $ ptrT"
    CppRef     -> pars ++ " $ refT"
    CppConst   -> pars ++ " $ constT"
    CppVirtual -> pars

surroundParams :: Bool -> String -> TypedName -> Bool -> String -> String
surroundParams overloaded suffix typed isConst pars = res where
  method    =  (if isConst then "mkConstMethod" else "mkMethod")
            ++ (if overloaded then "' " else " ")
  alterName = if overloaded then (\s -> s ++ "\" \"" ++ s ++ suffix) else id
  res       = method ++ goSurroundParams alterName typed pars

toQtahParams :: [TypedName] -> [String]
toQtahParams params = minQtahParams ++ maxQtahParams where
  minParams = mapMaybe (fromHeads . toQtahParam) params
  maxParams = map      (denom     . toQtahParam) params
  makeQtahParams ps = concat ["[", intercalate ", " ps , "]"]
  minQtahParams = [makeQtahParams minParams]
  maxQtahParams = [makeQtahParams maxParams | length minParams /= length maxParams]

toQtahDecl :: Bool -> FunDecl -> [String]
toQtahDecl overloaded1 (FunDecl result params isConst) = surrAllParams qtahParams where
  qtahParams = toQtahParams params
  surrParams overloaded2 = surroundParams (overloaded1 || overloaded2) suffix result isConst where
    suffix = if overloaded2 then "All" else ""
  surrAllParams = map (uncurry surrParams) . zip [False, True]

toQtahDecls :: [FunDecl] -> [String]
toQtahDecls [decl] = toQtahDecl False decl
toQtahDecls decls  = decls >>= toQtahDecl True

transformDecls :: [String] -> [String]
transformDecls = funDeclsFams >=> toQtahDecls where
  funDeclsFams = groupBy ((==) `on` getName . funResult) . map parseFunDecl

transformEnum :: String -> String
transformEnum s = concat ["(", val, ", ", trans name, ")"] where
  [name, val] = words s
  trans = show . map (map toLower) . continuousBy isUpper

transformDeclsOrEnums :: [String] -> [String]
transformDeclsOrEnums ds | any (elem '(') ds = transformDecls ds
transformDeclsOrEnums es                     = map transformEnum es

onNewlinedContents :: ([String] -> IO ()) -> IO ()
onNewlinedContents k = go k where
  go k' = do
    s <- getLine
    if null s
      then k' [] *> go k
      else go (k' . (s:))

main :: IO ()
main = onNewlinedContents $ \ss -> do
  mapM_ (putStrLn . ("  , " ++)) $ transformDeclsOrEnums ss
  putStrLn ""

-- For testing:

{-
FullViewportUpdate 0
MinimalViewportUpdate 1
SmartViewportUpdate 2
BoundingRectViewportUpdate 4
NoViewportUpdate 3
-}

{-
QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsPolygonItem * addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())
qreal height() const
qreal height(int blah) const
void invalidate(qreal x, qreal y, SceneLayers layers = AllLayers)
void invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)
-}

{-
bool 	acceptDrops() const
bool 	acceptHoverEvents() const
bool 	acceptTouchEvents() const
Qt::MouseButtons 	acceptedMouseButtons() const
virtual void 	advance(int phase)
virtual QRectF 	boundingRect() const = 0
QRegion 	boundingRegion(const QTransform & itemToDeviceTransform) const
qreal 	boundingRegionGranularity() const
CacheMode 	cacheMode() const
QList<QGraphicsItem *> 	childItems() const
QRectF 	childrenBoundingRect() const
void 	clearFocus()
QPainterPath 	clipPath() const
virtual bool 	collidesWithItem(const QGraphicsItem * other, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
virtual bool 	collidesWithPath(const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
QList<QGraphicsItem *> 	collidingItems(Qt::ItemSelectionMode mode = Qt::IntersectsItemShape) const
QGraphicsItem * 	commonAncestorItem(const QGraphicsItem * other) const
virtual bool 	contains(const QPointF & point) const
QCursor 	cursor() const
QVariant 	data(int key) const
QTransform 	deviceTransform(const QTransform & viewportTransform) const
qreal 	effectiveOpacity() const
void 	ensureVisible(const QRectF & rect = QRectF(), int xmargin = 50, int ymargin = 50)
void 	ensureVisible(qreal x, qreal y, qreal w, qreal h, int xmargin = 50, int ymargin = 50)
bool 	filtersChildEvents() const
GraphicsItemFlags 	flags() const
QGraphicsItem * 	focusItem() const
QGraphicsItem * 	focusProxy() const
void 	grabKeyboard()
void 	grabMouse()
QGraphicsEffect * 	graphicsEffect() const
QGraphicsItemGroup * 	group() const
bool 	hasCursor() const
bool 	hasFocus() const
void 	hide()
Qt::InputMethodHints 	inputMethodHints() const
void 	installSceneEventFilter(QGraphicsItem * filterItem)
bool 	isActive() const
bool 	isAncestorOf(const QGraphicsItem * child) const
bool 	isBlockedByModalPanel(QGraphicsItem ** blockingPanel = 0) const
bool 	isClipped() const
bool 	isEnabled() const
bool 	isObscured() const
bool 	isObscured(qreal x, qreal y, qreal w, qreal h) const
bool 	isObscured(const QRectF & rect) const
virtual bool 	isObscuredBy(const QGraphicsItem * item) const
bool 	isPanel() const
bool 	isSelected() const
bool 	isUnderMouse() const
bool 	isVisible() const
bool 	isVisibleTo(const QGraphicsItem * parent) const
bool 	isWidget() const
bool 	isWindow() const
QTransform 	itemTransform(const QGraphicsItem * other, bool * ok = 0) const
QPointF 	mapFromItem(const QGraphicsItem * item, const QPointF & point) const
QPolygonF 	mapFromItem(const QGraphicsItem * item, const QRectF & rect) const
QPolygonF 	mapFromItem(const QGraphicsItem * item, const QPolygonF & polygon) const
QPainterPath 	mapFromItem(const QGraphicsItem * item, const QPainterPath & path) const
QPolygonF 	mapFromItem(const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapFromItem(const QGraphicsItem * item, qreal x, qreal y) const
QPointF 	mapFromParent(const QPointF & point) const
QPolygonF 	mapFromParent(const QRectF & rect) const
QPolygonF 	mapFromParent(const QPolygonF & polygon) const
QPainterPath 	mapFromParent(const QPainterPath & path) const
QPolygonF 	mapFromParent(qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapFromParent(qreal x, qreal y) const
QPointF 	mapFromScene(const QPointF & point) const
QPolygonF 	mapFromScene(const QRectF & rect) const
QPolygonF 	mapFromScene(const QPolygonF & polygon) const
QPainterPath 	mapFromScene(const QPainterPath & path) const
QPolygonF 	mapFromScene(qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapFromScene(qreal x, qreal y) const
QRectF 	mapRectFromItem(const QGraphicsItem * item, const QRectF & rect) const
QRectF 	mapRectFromItem(const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h) const
QRectF 	mapRectFromParent(const QRectF & rect) const
QRectF 	mapRectFromParent(qreal x, qreal y, qreal w, qreal h) const
QRectF 	mapRectFromScene(const QRectF & rect) const
QRectF 	mapRectFromScene(qreal x, qreal y, qreal w, qreal h) const
QRectF 	mapRectToItem(const QGraphicsItem * item, const QRectF & rect) const
QRectF 	mapRectToItem(const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h) const
QRectF 	mapRectToParent(const QRectF & rect) const
QRectF 	mapRectToParent(qreal x, qreal y, qreal w, qreal h) const
QRectF 	mapRectToScene(const QRectF & rect) const
QRectF 	mapRectToScene(qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapToItem(const QGraphicsItem * item, const QPointF & point) const
QPolygonF 	mapToItem(const QGraphicsItem * item, const QRectF & rect) const
QPolygonF 	mapToItem(const QGraphicsItem * item, const QPolygonF & polygon) const
QPainterPath 	mapToItem(const QGraphicsItem * item, const QPainterPath & path) const
QPolygonF 	mapToItem(const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapToItem(const QGraphicsItem * item, qreal x, qreal y) const
QPointF 	mapToParent(const QPointF & point) const
QPolygonF 	mapToParent(const QRectF & rect) const
QPolygonF 	mapToParent(const QPolygonF & polygon) const
QPainterPath 	mapToParent(const QPainterPath & path) const
QPolygonF 	mapToParent(qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapToParent(qreal x, qreal y) const
QPointF 	mapToScene(const QPointF & point) const
QPolygonF 	mapToScene(const QRectF & rect) const
QPolygonF 	mapToScene(const QPolygonF & polygon) const
QPainterPath 	mapToScene(const QPainterPath & path) const
QPolygonF 	mapToScene(qreal x, qreal y, qreal w, qreal h) const
QPointF 	mapToScene(qreal x, qreal y) const
void 	moveBy(qreal dx, qreal dy)
qreal 	opacity() const
virtual QPainterPath 	opaqueArea() const
virtual void 	paint(QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * widget = 0) = 0
QGraphicsItem * 	panel() const
PanelModality 	panelModality() const
QGraphicsItem * 	parentItem() const
QGraphicsObject * 	parentObject() const
QGraphicsWidget * 	parentWidget() const
QPointF 	pos() const
void 	removeSceneEventFilter(QGraphicsItem * filterItem)
void 	resetTransform()
qreal 	rotation() const
qreal 	scale() const
QGraphicsScene * 	scene() const
QRectF 	sceneBoundingRect() const
QPointF 	scenePos() const
QTransform 	sceneTransform() const
void 	scroll(qreal dx, qreal dy, const QRectF & rect = QRectF())
void 	setAcceptDrops(bool on)
void 	setAcceptHoverEvents(bool enabled)
void 	setAcceptTouchEvents(bool enabled)
void 	setAcceptedMouseButtons(Qt::MouseButtons buttons)
void 	setActive(bool active)
void 	setBoundingRegionGranularity(qreal granularity)
void 	setCacheMode(CacheMode mode, const QSize & logicalCacheSize = QSize())
void 	setCursor(const QCursor & cursor)
void 	setData(int key, const QVariant & value)
void 	setEnabled(bool enabled)
void 	setFiltersChildEvents(bool enabled)
void 	setFlag(GraphicsItemFlag flag, bool enabled = true)
void 	setFlags(GraphicsItemFlags flags)
void 	setFocus(Qt::FocusReason focusReason = Qt::OtherFocusReason)
void 	setFocusProxy(QGraphicsItem * item)
void 	setGraphicsEffect(QGraphicsEffect * effect)
void 	setGroup(QGraphicsItemGroup * group)
void 	setInputMethodHints(Qt::InputMethodHints hints)
void 	setOpacity(qreal opacity)
void 	setPanelModality(PanelModality panelModality)
void 	setParentItem(QGraphicsItem * newParent)
void 	setPos(const QPointF & pos)
void 	setPos(qreal x, qreal y)
void 	setRotation(qreal angle)
void 	setScale(qreal factor)
void 	setSelected(bool selected)
void 	setToolTip(const QString & toolTip)
void 	setTransform(const QTransform & matrix, bool combine = false)
void 	setTransformOriginPoint(const QPointF & origin)
void 	setTransformOriginPoint(qreal x, qreal y)
void 	setTransformations(const QList<QGraphicsTransform *> & transformations)
void 	setVisible(bool visible)
void 	setX(qreal x)
void 	setY(qreal y)
void 	setZValue(qreal z)
virtual QPainterPath 	shape() const
void 	show()
void 	stackBefore(const QGraphicsItem * sibling)
QGraphicsObject * 	toGraphicsObject()
const QGraphicsObject * 	toGraphicsObject() const
QString 	toolTip() const
QGraphicsItem * 	topLevelItem() const
QGraphicsWidget * 	topLevelWidget() const
QTransform 	transform() const
QPointF 	transformOriginPoint() const
QList<QGraphicsTransform *> 	transformations() const
virtual int 	type() const
void 	ungrabKeyboard()
void 	ungrabMouse()
void 	unsetCursor()
void 	update(const QRectF & rect = QRectF())
void 	update(qreal x, qreal y, qreal width, qreal height)
QGraphicsWidget * 	window() const
qreal 	x() const
qreal 	y() const
qreal 	zValue() const
-}
