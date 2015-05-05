module SetsFin where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

data Element = I Int | S String | Is [Int] | Ss [String] | EmptyElement deriving (Eq, Ord)
instance Show Element where
  show (I i) = show i
  show (S s) = s
  show (Is is) = showIntList is
  show (Ss ss) = showStringList ss
  show EmptyElement = ""

showIntList :: [Int] -> String
showIntList is = "{" ++ showed ++ "}"
    where
      showed = drop 2 $ foldl (++) "" $ map (\e -> ", " ++ show e) is

showStringList :: [String] -> String
showStringList ss = "{" ++ showed ++ "}"
    where
      showed = drop 2 $ foldl (++) "" $ map (\e -> ", " ++ show e) ss

data Object = Object [Element] deriving (Eq, Ord)
instance Show Object where
  show (Object es) = "{" ++ showed ++ "}"
    where
      showed = drop 2 $ foldl (++) "" $ map (\e -> ", " ++ show e) es

data Arrow = Arrow Object Object (Map.Map Element Element) deriving (Eq, Ord)
instance Show Arrow where
  show (Arrow d c m) = "dom: " ++ (show d) ++ " cod: " ++ (show c) ++ " map: " ++ showed
    where
      showed = drop 2 $ foldl (++) "" arrowed
      arrowed = map (\(a,b) -> ", " ++ show a ++ "->"++ show b) $ Map.toList m

data SetsFin = SetsFin [Object] [Arrow] deriving Show

printSetsFin :: SetsFin -> IO ()
printSetsFin (SetsFin objs arrows) = do
  putStrLn "Objects"
  printObjects objs
  putStrLn "Arrows"
  printArrows arrows

printObjects :: [Object] -> IO ()
printObjects objs = do
  mapM_ (\obj -> putStrLn $ show obj) objs

printArrows :: [Arrow] -> IO ()
printArrows as = do
  mapM_ (\a -> putStrLn $ show a) as

toObject :: [Element] -> Object
toObject es = Object $ Set.toList $ Set.fromList es

toObjects :: [[Element]] -> [Object]
toObjects ess = map toObject ess

empty :: Object
empty = Object [EmptyElement]

elements :: Object -> [Element]
elements (Object es) = es

equals :: Object -> Object -> Bool
equals (Object o1) (Object o2) = length o1 == length o2 && hasSameElements
  where
    hasSameElements = and $ map (\(e1, e2) -> e1 == e2) tupled
    tupled = zip sorted1 sorted2
    sorted1 = List.sort o1
    sorted2 = List.sort o2

justArrows :: [Maybe Arrow] -> [Arrow]
justArrows arrows =  map maybeToArrow jarrows
  where
    jarrows = filter (\m -> Maybe.isJust m) arrows

maybeToArrow :: Maybe Arrow -> Arrow
maybeToArrow (Just a) = a
maybeToArrow Nothing = Arrow empty empty $ Map.fromList [(EmptyElement, EmptyElement)]

hom_ :: SetsFin -> Object -> [Arrow]
hom_ (SetsFin _ arrows) from = filter (\a -> (dom a) `equals` from) arrows

hom :: SetsFin -> Object -> Object -> [Arrow]
hom (SetsFin _ arrows) from to = filter (\a -> (dom a) `equals` from && (cod a) `equals` to) arrows

toArrow :: Object -> Object -> Arrow
toArrow (Object o1) (Object o2) = Arrow (Object o1) (Object o2) $ Map.fromList (zip o1 o2)

emptyArrow :: Object -> Arrow
emptyArrow (Object es) = Arrow empty (Object es) $ Map.empty

isEmptyArrow :: Arrow -> Bool
isEmptyArrow (Arrow d _ _) = d == empty

dom :: Arrow -> Object
dom (Arrow d _ _) = d

cod :: Arrow -> Object
cod (Arrow _ c _) = c

compose :: Arrow -> Arrow -> Maybe Arrow
compose a1 a2 = if cod a1 == dom a2
  then if isEmptyArrow a1
    then Just $ emptyArrow $ cod a2
    else Just $ Arrow (dom a1) (cod a2) $ composeMap a1 a2
  else Nothing

composeMap :: Arrow -> Arrow -> (Map.Map Element Element)
composeMap (Arrow d1 _ m1) (Arrow _ _ m2) = Map.fromList composed
  where
    composed = map (\e -> (e, lookupElement e m1 m2)) $ elements d1

lookupElement :: Element -> (Map.Map Element Element) -> (Map.Map Element Element) -> Element
lookupElement e m1 m2 = justLookup (justLookup e m1) m2
  where
    justLookup e2 m = Maybe.fromJust $ Map.lookup e2 m

generate :: [Object] -> SetsFin
generate xs = let trimed = trimObjects xs
  in
    SetsFin (empty : trimed) $ (arrowsFromEmpty trimed) ++ (makeArrows trimed)

generateFromInt :: [Int] -> SetsFin
generateFromInt is = generateFromElement $ map (\i -> I i) is

generateFromElement :: [Element] -> SetsFin
generateFromElement es = generate objs
  where
    objs = trimObjects seqs
    seqs = toObjects $ List.subsequences es

trimObjects :: [Object] -> [Object]
trimObjects = Set.toList . Set.fromList . filter (\(Object es2) -> (not $ List.null es2) && es2 /= [EmptyElement])

generateFromString :: [String] -> SetsFin
generateFromString ss = generateFromElement $ map (\e -> S e) ss

generateFromIntList :: [[Int]] -> SetsFin
generateFromIntList ils = generateFromElement $ map (\e -> Is e) ils

generateFromStringList :: [[String]] -> SetsFin
generateFromStringList sls = generateFromElement $ map (\e -> Ss e) sls

getObjects :: SetsFin -> [Object]
getObjects (SetsFin objs _) = objs

getArrows :: SetsFin -> [Arrow]
getArrows (SetsFin _ as) = as

validate :: SetsFin -> IO ()
validate (SetsFin objs as) = do
  checkIdentity objs as
  checkComposite objs as
  checkAssociativity objs as

checkIdentity :: [Object] -> [Arrow] -> IO ()
checkIdentity objs as = do
  putStrLn "Check indentity"
  noObjs <- noIdentity
  if null noObjs
    then putStrLn "OK. All objects have identity"
    else do
      putStrLn "Following objects have no identity"
      mapM_ (\obj -> putStrLn $ show $ obj) noObjs
      where
        noIdentity = return $ map fst $ filter (\(_, i) -> not $ elem i as) ids
        ids = map (\obj -> (obj, makeIdentity obj)) objs

makeIdentity :: Object -> Arrow
makeIdentity obj = if obj == empty
  then emptyArrow obj
  else Arrow obj obj $ Map.fromList (zip es es)
  where
    es = elements obj

checkComposite :: [Object] -> [Arrow] -> IO ()
checkComposite _ as = do
  putStrLn "Check composite"
  let missing = missingArrows as
  if null missing
    then putStrLn "OK, All composites are included"
    else do
      putStrLn "Following arrows are missing"
      mapM_ showTuple missing
        where
          showTuple (from, to, composed) = do
            putStrLn $ (show composed) ++ " (as a composite of " ++ (show from) ++ " and " ++ (show to) ++ ")"

missingArrows :: [Arrow] -> [(Arrow, Arrow, Arrow)]
missingArrows as = missing
  where
    missing = map (\(a, b, mc) -> (a, b, Maybe.fromJust mc)) missingM
    missingM = filter isMissing composed
    isMissing (_, _, mc) = case mc of
      Nothing -> False
      Just a -> not $ elem a as
    composed = map (\(from, to) -> (from, to, compose from to)) tupled
    byCod = groupByCodomain as
    tupled = concat $ map tupleFromTo as
    tupleFromTo from = case Map.lookup (cod from) byCod of
      Nothing -> []
      Just bycod -> map (\to -> (from, to)) bycod

groupByDomain :: [Arrow] -> (Map.Map Object [Arrow])
groupByDomain as = Map.fromList ls
  where
    ls = map (\g -> (dom (head g), g)) grouped
    grouped = List.groupBy (\a b -> dom a == dom b) sorted
    sorted = List.sortBy (\a b -> dom a `compare` dom b) as

groupByCodomain :: [Arrow] -> (Map.Map Object [Arrow])
groupByCodomain as = Map.fromList ls
  where
    ls = map (\g -> (cod (head g), g)) grouped
    grouped = List.groupBy (\a b -> cod a == cod b) sorted
    sorted = List.sortBy (\a b -> cod a `compare` cod b) as

checkAssociativity :: [Object] -> [Arrow] -> IO ()
checkAssociativity _ as = do
  putStrLn "check assosiativity"
  let broken = brokenAssociativity as
  if null broken
     then putStrLn "OK"
     else do
      putStrLn "Following arrows are missing"
      mapM_ (\(_, _, a) -> putStrLn $ show a) broken

brokenAssociativity :: [Arrow] -> [(Maybe Arrow, Maybe Arrow, (Arrow, Arrow, Arrow))]
brokenAssociativity as = broken
  where
   broken = filter (\(l, r, _) -> l /= r) composed
   composed = map (\t -> (left t, right t, t)) composable
   left (a, b, c) = compose2 (Just a) (compose b c)
   right (a, b, c) = compose2 (compose a b) (Just c)
   compose2 ml mr = do
     l <- ml
     r <- mr
     compose l r
   byCod = groupByCodomain as
   composable = concat $ map makeComposable tupled
   makeComposable (a, b) = case Map.lookup (cod b) byCod of
     Nothing -> []
     Just bycod -> map (\c -> (a, b, c)) bycod
   tupled = concat $ map makeTuple as
   makeTuple a = case Map.lookup (cod a) byCod of
     Nothing -> []
     Just bycod -> map (\b -> (a, b)) bycod

makeArrows :: [Object] -> [Arrow]
makeArrows objs = all1 ++ all2
  where
    all1 = concat $ map makeAll1 smaller
    all2 = concat $ map makeAll2 notSmaller
    makeAll1 (obj1, obj2) = arrowsToSmaller obj1 obj2
    makeAll2 (obj1, obj2) = arrowsNotToSmaller obj1 obj2
    smaller = filter (\((Object obj1), (Object obj2)) -> length obj1 > length obj2) objectPair
    notSmaller = filter (\((Object obj1), (Object obj2)) -> length obj1 <= length obj2) objectPair
    objectPair = allPair objs

arrowsFromEmpty :: [Object] -> [Arrow]
arrowsFromEmpty objs = map (\obj -> emptyArrow obj) (empty : objs)

allPair :: [Object] -> [(Object, Object)]
allPair objs = [(obj1, obj2) | obj1 <- objs, obj2 <- objs]

arrowsToSmaller :: Object -> Object -> [Arrow]
arrowsToSmaller (Object from) (Object to) = arrows
  where
    arrows = map (\m -> Arrow (Object from) (Object to) m) maps
    maps = map (\m -> Maybe.fromJust m) $ filter (\m -> Maybe.isJust m) maybeMaps
    maybeMaps = map (\xs -> makeOneToOne from xs) cods
    cods = fillUp (Object to) (length from)

arrowsNotToSmaller :: Object -> Object -> [Arrow]
arrowsNotToSmaller (Object from) (Object to) = arrows
  where
    arrows = map (\m -> Arrow (Object from) (Object to) m) maps
    maps = map (\m -> Maybe.fromJust m) $ filter (\m -> Maybe.isJust m) maybeMaps
    maybeMaps = map (\xs -> makeOneToOne from xs) cods
    cods = fillUp (Object to) (length from)

fillUp :: Object -> Int -> [[Element]]
fillUp (Object obj) n = ifillUp 0 [[]]
  where
    ifillUp i res = if n <= i
      then res
      else ifillUp (i + 1) appended
      where
        appended = concat $ map (\xs -> appendElements xs obj) res

appendElements :: [Element] -> [Element] -> [[Element]]
appendElements to from = map (\x -> (x : to)) from

makeOneToOne :: [Element] -> [Element] -> Maybe (Map.Map Element Element)
makeOneToOne from to = if (length from == length to)
  then Just $ Map.fromList $ zip from to
  else Nothing
{-
tobj1 :: Object
tobj1 = Object [I 1, I 2, I 3]
tobj2 :: Object
tobj2 = Object [S "a", S "b"]
tobj3 :: Object
tobj3 = Object [I 1]
tobj4 :: Object
tobj4 = Object [I 2, I 4]
tobj5 :: Object
tobj5 = Object [Is [1,2,3], Is [3,4], Is [1,1]]

tsets :: SetsFin
tsets = generate [tobj1, tobj2, tobj3, tobj4]

farr1 = hom_ tsets tobj1
farr2 = hom_ tsets tobj2
farr3 = hom_ tsets tobj3
farr4 = hom_ tsets tobj4

farr1cods = Set.fromList $ map (\a -> cod a) farr1
farr2cods = Set.fromList $ map (\a -> cod a) farr2
farr3cods = Set.fromList $ map (\a -> cod a) farr3
farr4cods = Set.fromList $ map (\a -> cod a) farr4

test1 = length (arrowsNotToSmaller tobj4 tobj3) == 0
test2 = length (arrowsNotToSmaller tobj2 tobj4) == 4
test3 = length (arrowsNotToSmaller tobj2 tobj1) == 9

tval = validate $ generateFromInt [1,2,3]
tval2 = validate $ SetsFin (getObjects tsets) (drop 20 (getArrows tsets))
-}
