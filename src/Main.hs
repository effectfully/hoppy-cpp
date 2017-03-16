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
parseModif "&"     = Just CppRef
parseModif "*"     = Just CppPtr
parseModif "const" = Just CppConst
parseModif _       = Nothing

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
toQtahModif CppOpt   = Nothing
toQtahModif CppPtr   = Just "ptrT"
toQtahModif CppRef   = Just "refT"
toQtahModif CppConst = Just "constT"

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
    CppOpt   -> goSurroundParams alterName rest  pars
    CppPtr   -> goSurroundParams alterName rest (pars ++ " $ ptrT")
    CppRef   -> goSurroundParams alterName rest (pars ++ " $ refT")
    CppConst -> error "goSurroundParams: CppConst"

surroundParams :: Bool -> String -> TypedName -> String -> String
surroundParams overloaded suffix typed pars = res where
  (premethod, typed') = case typed of
    CppModif CppConst rest -> ("mkConstMethod", rest )
    _                      -> ("mkMethod"     , typed)
  method    = premethod ++ if overloaded then "' " else " "
  alterName = if overloaded then (\s -> s ++ "\" \"" ++ s ++ suffix) else id
  res       = method ++ goSurroundParams alterName typed' pars

toQtahParams :: [TypedName] -> [String]
toQtahParams params = minQtahParams ++ maxQtahParams where
  minParams = mapMaybe (coinToMaybe . toQtahParam) params
  maxParams = map      (denom       . toQtahParam) params
  makeQtahParams ps = concat ["[", intercalate ", " ps , "]"]
  minQtahParams = [makeQtahParams minParams]
  maxQtahParams = [makeQtahParams maxParams | length minParams /= length maxParams]

toQtahDecl :: Bool -> FunDecl -> [String]
toQtahDecl overloaded1 (FunDecl result params) = surrAllParams qtahParams where
  qtahParams = toQtahParams params
  surrParams overloaded2 = surroundParams (overloaded1 || overloaded2) suffix result where
    suffix = if overloaded2 then "All" else ""
  surrAllParams = map (uncurry surrParams) . zip [False, True]

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

-- For testing:

{-
FullViewportUpdate
MinimalViewportUpdate
SmartViewportUpdate
BoundingRectViewportUpdate
NoViewportUpdate
-}

{-
QGraphicsEllipseItem *  addEllipse(qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush())
QGraphicsPolygonItem * addPolygon(const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush())
qreal height() const
qreal height(int blah) const
void invalidate(qreal x, qreal y, SceneLayers layers = AllLayers)
void invalidate(qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers)
-}



