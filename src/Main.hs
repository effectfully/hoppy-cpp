{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Function
import Data.Char
import Data.Maybe
import Data.List
import Data.Bifunctor
import Control.Monad

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

tag :: (a -> b) -> a -> (a, b)
tag f x = (x, f x)
{-# INLINE tag #-}

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
splitBy p [] = []
splitBy p xs = go xs where
  go xs = pxs : onCons (const go) sxs where
    (pxs, sxs) = break p xs
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

tailsify :: Coin a -> Coin a
tailsify = Tails . denom

coinToMaybe :: Coin a -> Maybe a
coinToMaybe (Heads x) = Just x
coinToMaybe (Tails _) = Nothing

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
  { funResult :: TypedName
  , funParams :: [TypedName]
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
parseFunDecl str = FunDecl result params where
  (preresult, '(':rest ) = break  (== '(') str
  (comParams, ')':modif) = breakR (== ')') rest
  result = growType (filter (not . isSpace) modif) $ parseTypedName preresult
  params = map parseTypedName $ splitBy (== ',') comParams

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
toQtahModif CppVirtual = error "toQtahModif: CppVirtual"

-- optional -> Tails.
toQtahParam :: TypedName -> Coin String
toQtahParam (CppPure  name)       = Heads name
toQtahParam (CppPrim  prim  rest) = Heads $ toQtahPrim prim
toQtahParam (CppClass cl    rest) = Heads $ "objT c_" ++ cl
toQtahParam (CppModif modif rest) = case toQtahModif modif of
  Nothing        -> tailsify $ toQtahParam rest
  Just qtahModif -> (\qtahRest -> qtahModif ++ " $ " ++ qtahRest) <$> toQtahParam rest

goSurroundParams :: (String -> String) -> TypedName -> String -> String
goSurroundParams alterName (CppPure  name)       pars =
  concat ["\"", alterName name, "\" ", pars]
goSurroundParams alterName (CppPrim  prim  rest) pars =
  goSurroundParams alterName rest pars ++ " " ++ toQtahPrim prim
goSurroundParams alterName (CppClass cl    rest) pars =
  goSurroundParams alterName rest (pars ++ " $ objT c_" ++ cl)
goSurroundParams alterName (CppModif modif rest) pars =
  case modif of
    CppOpt     -> goSurroundParams alterName rest  pars
    CppPtr     -> goSurroundParams alterName rest (pars ++ " $ ptrT")
    CppRef     -> goSurroundParams alterName rest (pars ++ " $ refT")
    CppConst   -> error "goSurroundParams: CppConst"
    CppVirtual -> error "goSurroundParams: CppVirtual"

surroundParams :: Bool -> TypedName -> String -> String
surroundParams overloaded typed pars = res where
  (premethod, typed') = case typed of
    CppModif CppConst rest -> ("mkConstMethod", rest )
    _                      -> ("mkMethod"     , typed)
  method    = premethod ++ if overloaded then "' " else " "
  alterName = if overloaded then (\s -> s ++ "\" \"" ++ s ++ "All") else id
  res       = method ++ goSurroundParams alterName typed' pars

toQtahParams :: [TypedName] -> [String]
toQtahParams params = minQtahParams ++ maxQtahParams where
  minParams = mapMaybe (coinToMaybe . toQtahParam) params
  maxParams = map      (denom       . toQtahParam) params
  makeQtahParams ps = concat ["[", intercalate ", " ps , "]"]
  minQtahParams = [makeQtahParams minParams]
  maxQtahParams = [makeQtahParams maxParams | length minParams /= length maxParams]

toQtahDecl :: Bool -> FunDecl -> [String]
toQtahDecl overloaded1 (FunDecl result params) =
    map (\(overloaded2, pars) -> surroundParams (overloaded1 || overloaded2) result pars)
  . zip [False, True]
  $ toQtahParams params

toQtahDecls :: [FunDecl] -> [String]
toQtahDecls [decl] = toQtahDecl False decl
toQtahDecls decls  = decls >>= toQtahDecl True

transformDecls :: [String] -> [String]
transformDecls = funDeclsFams >=> toQtahDecls where
  funDeclsFams = groupBy ((==) `on` getName . funResult) . map parseFunDecl

transformEnum :: String -> String
transformEnum = show . map (map toLower) . continuousBy isUpper

transformDeclsOrEnums :: [String] -> [String]
transformDeclsOrEnums ds | any (elem ' ') ds = transformDecls ds
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
  mapM_ putStrLn $ transformDeclsOrEnums ss
  putStrLn ""







{-
FullViewportUpdate
MinimalViewportUpdate
SmartViewportUpdate
BoundingRectViewportUpdate
NoViewportUpdate
-}


breakMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
breakMaybe f = go where
  go []             =  ([], [])
  go xs@(x:xs')
    | Just y <- f x = first (y:) $ breakMaybe f xs'
    | otherwise     = ([],xs)
{-# INLINE breakMaybe #-}

splitMaybe :: (a -> Maybe b) -> [a] -> [[b]]
splitMaybe f [] = []
splitMaybe f xs = go xs where
  go xs = pxs : onCons (const go) sxs where
    (pxs, sxs) = breakMaybe f xs
{-# INLINE splitMaybe #-}

{-continuousMaybe :: (a -> Maybe b) -> [a] -> [[b]]
continuousMaybe f = go where
  go xs = [pxs | not $ null pxs] ++ onCons (\x -> go) sxs where
    (pxs, sxs) = breakMaybe f xs
{-# INLINE continuousMaybe #-}-}



{-transformDeclOrEnum :: String -> Maybe String
transformDeclOrEnum ""               = Nothing
transformDeclOrEnum s | ' ' `elem` s = Just $ transformDecl s
transformDeclOrEnum s                = Just $ transformEnum s-}


{-main :: IO ()
main = do
  s <- getContents
  -- mapM_ (\ss -> putStrLn "" *> mapM_ putStrLn ss) $ splitMaybe transformDeclOrEnum $ lines s
  mapM_ (putStrLn . transformDeclOrEnum) $ lines s-}





{-main :: IO ()
main = do
  putStrLn $ transformDecl "QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())"
  -- putStrLn . toQtahDecl $ parseFunDecl "virtual QVariant inputMethodQuery(Qt::InputMethodQuery query) const"
  putStrLn $ transformDecl "QGraphicsProxyWidget *  addWidget(QWidget * widget, Qt::WindowFlags wFlags = 0)"

  putStrLn $ transformDecl "QGraphicsRectItem * addRect(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())"

  putStrLn $ transformDecl "QGraphicsPolygonItem * addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())"

  putStrLn $ transformDecl "qreal height() const"

  putStrLn $ transformDecl "void invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)"-}


{-
QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsPolygonItem * addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())
qreal height() const
qreal height(int blah) const
void invalidate(qreal x, qreal y, SceneLayers layers = AllLayers)
void invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)
-}


-- QFont 	font() const
-- QBrush 	foregroundBrush() const
-- bool 	hasFocus() const
-- qreal 	height() const
-- virtual QVariant 	inputMethodQuery(Qt::InputMethodQuery query) const
-- void 	invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)
-- bool 	isActive() const


{-
data CppPrim
  = CppVoid
  | CppBool
  | CppInt
  | CppFloat
  | CppDouble
  deriving (Show)

data CppModif
  = CppPtr
  | CppRef
  | CppConst
  | CppVirtual
  deriving (Eq, Ord, Show)

data CppType a
  = CppPure  a
  | CppPrim  CppPrim  (CppType a)
  | CppClass String   (CppType a)
  | CppModif CppModif (CppType a)
  deriving (Show)
-}


{-
FunDecl {funResult = CppPrim CppVoid (CppPure "invalidate"), funParams = [CppPrim CppDouble (CppPure "x"),CppPrim CppDouble (CppPure "y"),CppPrim CppDouble (CppPure "w"),CppPrim CppDouble (CppPure "h"),CppClass "SceneLayers" (CppPure "layers")]}
-}

{-
  [ mkCtor "new" [objT c_QPointF, objT c_QPointF, objT c_QPointF]
  , mkConstMethod "globalPos" [] $ objT c_QPoint
  , mkConstMethod "globalX" [] intT
-}






{-QGraphicsScene(QObject * parent = 0)
 QGraphicsScene(const QRectF & sceneRect, QObject * parent = 0)
 QGraphicsScene(qreal x, qreal y, qreal width, qreal height, QObject * parent = 0)
virtual  ~QGraphicsScene()
QGraphicsItem *  activePanel() const
QGraphicsWidget *  activeWindow() const
QGraphicsEllipseItem *  addEllipse(const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
void  addItem(QGraphicsItem * item)
QGraphicsLineItem *  addLine(const QLineF & line, const QPen & pen = QPen())
QGraphicsLineItem *  addLine(qreal x1, qreal y1, qreal x2, qreal y2, const QPen & pen = QPen())
QGraphicsPathItem *  addPath(const QPainterPath & path, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsPixmapItem *  addPixmap(const QPixmap & pixmap)
QGraphicsPolygonItem *  addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsRectItem *  addRect(const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsRectItem *  addRect(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsSimpleTextItem *  addSimpleText(const QString & text, const QFont & font = QFont())
QGraphicsTextItem *  addText(const QString & text, const QFont & font = QFont())
QGraphicsProxyWidget *  addWidget(QWidget * widget, Qt::WindowFlags wFlags = 0)-}






initLast :: [a] -> ([a], a)
initLast  []    = error "initLast: empty list"
initLast  [x]   = ([], x)
initLast (x:xs) = first (x:) $ initLast xs





{-
mkConstMethod "alignment" [] $ objT c_Qt::Alignment
mkConstMethod "backgroundBrush" [] $ objT c_QBrush
mkConstMethod "cacheMode" [] $ objT c_CacheMode
mkMethod' "centerOn" "centerOnAll" [objT c_QPointF] voidT
mkMethod' "centerOn" "centerOnAll" [doubleT, doubleT] voidT
mkMethod' "centerOn" "centerOnAll" [ptrT $ constT $ objT c_QGraphicsItem] voidT
mkConstMethod "dragMode" [] $ objT c_DragMode
mkMethod' "ensureVisible" "ensureVisibleAll" [objT c_QRectF] voidT
mkMethod' "ensureVisible" "ensureVisibleAll" [objT c_QRectF, intT, intT] voidT
mkMethod' "ensureVisible" "ensureVisibleAll" [doubleT, doubleT, doubleT, doubleT] voidT
mkMethod' "ensureVisible" "ensureVisibleAll" [doubleT, doubleT, doubleT, doubleT, intT, intT] voidT
mkMethod' "ensureVisible" "ensureVisibleAll" [ptrT $ constT $ objT c_QGraphicsItem] voidT
mkMethod' "ensureVisible" "ensureVisibleAll" [ptrT $ constT $ objT c_QGraphicsItem, intT, intT] voidT
mkMethod' "fitInView" "fitInViewAll" [objT c_QRectF] voidT
mkMethod' "fitInView" "fitInViewAll" [objT c_QRectF, objT c_Qt::AspectRatioMode] voidT
mkMethod' "fitInView" "fitInViewAll" [doubleT, doubleT, doubleT, doubleT] voidT
mkMethod' "fitInView" "fitInViewAll" [doubleT, doubleT, doubleT, doubleT, objT c_Qt::AspectRatioMode] voidT
mkMethod' "fitInView" "fitInViewAll" [ptrT $ constT $ objT c_QGraphicsItem] voidT
mkMethod' "fitInView" "fitInViewAll" [ptrT $ constT $ objT c_QGraphicsItem, objT c_Qt::AspectRatioMode] voidT
mkConstMethod "foregroundBrush" [] $ objT c_QBrush
mkConstMethod "isInteractive" [] boolT
mkConstMethod "isTransformed" [] boolT
mkConstMethod' "itemAt" "itemAtAll" [objT c_QPoint] $ ptrT $ objT c_QGraphicsItem
mkConstMethod' "itemAt" "itemAtAll" [intT, intT] $ ptrT $ objT c_QGraphicsItem
mkConstMethod' "items" "itemsAll" [] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QPoint] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [intT, intT] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [intT, intT, intT, intT] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [intT, intT, intT, intT, objT c_Qt::ItemSelectionMode] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QRect] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QRect, objT c_Qt::ItemSelectionMode] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QPolygon] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QPolygon, objT c_Qt::ItemSelectionMode] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QPainterPath] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "items" "itemsAll" [objT c_QPainterPath, objT c_Qt::ItemSelectionMode] $ objT c_QList<QGraphicsItem $ objT c_*>
mkConstMethod' "mapFromScene" "mapFromSceneAll" [objT c_QPointF] $ objT c_QPoint
mkConstMethod' "mapFromScene" "mapFromSceneAll" [objT c_QRectF] $ objT c_QPolygon
mkConstMethod' "mapFromScene" "mapFromSceneAll" [objT c_QPolygonF] $ objT c_QPolygon
mkConstMethod' "mapFromScene" "mapFromSceneAll" [objT c_QPainterPath] $ objT c_QPainterPath
mkConstMethod' "mapFromScene" "mapFromSceneAll" [doubleT, doubleT] $ objT c_QPoint
mkConstMethod' "mapFromScene" "mapFromSceneAll" [doubleT, doubleT, doubleT, doubleT] $ objT c_QPolygon
mkConstMethod' "mapToScene" "mapToSceneAll" [objT c_QPoint] $ objT c_QPointF
mkConstMethod' "mapToScene" "mapToSceneAll" [objT c_QRect] $ objT c_QPolygonF
mkConstMethod' "mapToScene" "mapToSceneAll" [objT c_QPolygon] $ objT c_QPolygonF
mkConstMethod' "mapToScene" "mapToSceneAll" [objT c_QPainterPath] $ objT c_QPainterPath
mkConstMethod' "mapToScene" "mapToSceneAll" [intT, intT] $ objT c_QPointF
mkConstMethod' "mapToScene" "mapToSceneAll" [intT, intT, intT, intT] $ objT c_QPolygonF
mkConstMethod "matrix" [] $ objT c_QMatrix
mkConstMethod "optimizationFlags" [] $ objT c_OptimizationFlags
mkMethod "render" [ptrT $ objT c_QPainter] voidT
mkMethod' "render" "renderAll" [ptrT $ objT c_QPainter, objT c_QRectF, objT c_QRect, objT c_Qt::AspectRatioMode] voidT
mkConstMethod "renderHints" [] $ objT c_QPainter::RenderHints
mkMethod "resetCachedContent" [] voidT
mkMethod "resetMatrix" [] voidT
mkMethod "resetTransform" [] voidT
mkConstMethod "resizeAnchor" [] $ objT c_ViewportAnchor
mkMethod "rotate" [doubleT] voidT
mkConstMethod "rubberBandSelectionMode" [] $ objT c_Qt::ItemSelectionMode
mkMethod "scale" [doubleT, doubleT] voidT
mkConstMethod "scene" [] $ ptrT $ objT c_QGraphicsScene
mkConstMethod "sceneRect" [] $ objT c_QRectF
mkMethod "setAlignment" [objT c_Qt::Alignment] voidT
mkMethod "setBackgroundBrush" [objT c_QBrush] voidT
mkMethod "setCacheMode" [objT c_CacheMode] voidT
mkMethod "setDragMode" [objT c_DragMode] voidT
mkMethod "setForegroundBrush" [objT c_QBrush] voidT
mkMethod "setInteractive" [boolT] voidT
mkMethod "setMatrix" [objT c_QMatrix] voidT
mkMethod' "setMatrix" "setMatrixAll" [objT c_QMatrix, boolT] voidT
mkMethod "setOptimizationFlag" [objT c_OptimizationFlag] voidT
mkMethod' "setOptimizationFlag" "setOptimizationFlagAll" [objT c_OptimizationFlag, boolT] voidT
mkMethod "setOptimizationFlags" [objT c_OptimizationFlags] voidT
mkMethod "setRenderHint" [objT c_QPainter::RenderHint] voidT
mkMethod' "setRenderHint" "setRenderHintAll" [objT c_QPainter::RenderHint, boolT] voidT
mkMethod "setRenderHints" [objT c_QPainter::RenderHints] voidT
mkMethod "setResizeAnchor" [objT c_ViewportAnchor] voidT
mkMethod "setRubberBandSelectionMode" [objT c_Qt::ItemSelectionMode] voidT
mkMethod "setScene" [ptrT $ objT c_QGraphicsScene] voidT
mkMethod' "setSceneRect" "setSceneRectAll" [objT c_QRectF] voidT
mkMethod' "setSceneRect" "setSceneRectAll" [doubleT, doubleT, doubleT, doubleT] voidT
mkMethod "setTransform" [objT c_QTransform] voidT
mkMethod' "setTransform" "setTransformAll" [objT c_QTransform, boolT] voidT
mkMethod "setTransformationAnchor" [objT c_ViewportAnchor] voidT
mkMethod "setViewportUpdateMode" [objT c_ViewportUpdateMode] voidT
mkMethod "shear" [doubleT, doubleT] voidT
mkConstMethod "transform" [] $ objT c_QTransform
mkConstMethod "transformationAnchor" [] $ objT c_ViewportAnchor
mkMethod "translate" [doubleT, doubleT] voidT
mkConstMethod "viewportTransform" [] $ objT c_QTransform
mkConstMethod "viewportUpdateMode" [] $ objT c_ViewportUpdateMode
-}
