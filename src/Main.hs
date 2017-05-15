{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Function
import Data.Char
import Data.Maybe
import Data.List
import Data.IORef
import Control.Monad

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .* f = \x y -> g (f x y)
{-# INLINE (.*) #-}

lastDef :: a -> [a] -> a
lastDef x [] = x
lastDef _ xs = last xs
{-# INLINE lastDef #-}

onCons :: (a -> [a] -> [b]) -> [a] -> [b]
onCons _  []    = []
onCons f (x:xs) = f x xs
{-# INLINE onCons #-}

onHead :: (a -> a) -> [a] -> [a]
onHead _  []    = []
onHead f (x:xs) = f x : xs
{-# INLINE onHead #-}

continuousBy :: (a -> Bool) -> [a] -> [[a]]
continuousBy _ [] = []
continuousBy p xs = go xs where
  go []  = [[]]
  go xs' = [pxs | not $ null pxs] ++ onCons (\x -> onHead (x:) . go) sxs where
    (pxs, sxs) = break p xs'
{-# INLINE continuousBy #-}

continuousFrom :: (a -> Bool) -> [a] -> [[a]]
continuousFrom p xs = go [] xs where
  go ps xs' = [pxs' | not $ null pxs'] ++ onCons (\x -> go [x]) sxs where
    (pxs, sxs) = break p xs'
    pxs' = ps ++ pxs
{-# INLINE continuousFrom #-}

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

dropBefore :: Eq a => [a] -> [a] -> [a]
dropBefore xs ys = lastDef ys . mapMaybe (stripPrefix xs) $ tails ys
{-# INLINE dropBefore #-}

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
parseTypedName s = normType $ gfoldr1 growType acc lexemes where
  (noopt, eqopt) = break (== '=') s
  isName c = isLetter c || any (c ==) "<>:"
  lexemes = words noopt >>= continuousBy isName
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
  name : val : _ = words s
  trans = ("[" ++)
        . (++ "]")
        . intercalate ", "
        . map (show . map toLower)
        . continuousFrom isUpper
        . dropBefore "::"

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
main = do
  mode <- newIORef True
  onNewlinedContents $ \ss -> do
    case ss of
      ["method"] -> writeIORef mode True  *> putStrLn "OK"
      ["enum"]   -> writeIORef mode False *> putStrLn "OK"
      _          -> do
        decl <- readIORef mode
        mapM_ (putStrLn . ("  , " ++)) $ if decl
          then transformDecls ss
          else map transformEnum ss
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
QGraphicsView::FullViewportUpdate	0	When any visible part of the scene changes or is reexposed, QGraphicsView will update the entire viewport. This approach is fastest when QGraphicsView spends more time figuring out what to draw than it would spend drawing (e.g., when very many small items are repeatedly updated). This is the preferred update mode for viewports that do not support partial updates, such as QGLWidget, and for viewports that need to disable scroll optimization.
QGraphicsView::MinimalViewportUpdate	1	QGraphicsView will determine the minimal viewport region that requires a redraw, minimizing the time spent drawing by avoiding a redraw of areas that have not changed. This is QGraphicsView's default mode. Although this approach provides the best performance in general, if there are many small visible changes on the scene, QGraphicsView might end up spending more time finding the minimal approach than it will spend drawing.
QGraphicsView::SmartViewportUpdate	2	QGraphicsView will attempt to find an optimal update mode by analyzing the areas that require a redraw.
QGraphicsView::BoundingRectViewportUpdate	4	The bounding rectangle of all changes in the viewport will be redrawn. This mode has the advantage that QGraphicsView searches only one region for changes, minimizing time spent determining what needs redrawing. The disadvantage is that areas that have not changed also need to be redrawn.
QGraphicsView::NoViewportUpdate	3	QGraphicsView will never update its viewport when the scene changes; the user is expected to control all updates. This mode disables all (potentially slow) item visibility testing in QGraphicsView, and is suitable for scenes that either require a fixed frame rate, or where the viewport is otherwise updated externally.
-}

{-
QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsPolygonItem * addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())
qreal height() const
qreal height(int blah) const
void invalidate(qreal x, qreal y, SceneLayers layers = AllLayers)
void invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)
void 	addItem(QGraphicsItem *item)
QGraphicsItem *	itemAt(const QPointF &position, const QTransform &deviceTransform) const
void 	focusItemChanged(QGraphicsItem *newFocusItem, QGraphicsItem *oldFocusItem, Qt::FocusReason reason)
void 	changed(const QList<QRectF> &region)
-}
