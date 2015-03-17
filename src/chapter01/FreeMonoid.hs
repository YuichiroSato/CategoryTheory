module FreeMonoid where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

data Alphabet = Alphabet (Set.Set String)
instance Show Alphabet where
    show (Alphabet al) = "Alphabet : " ++ stral
        where stral = foldC $ map show' $ Set.toList al

type Word = String
type Concatenation = Map.Map (Word, Word) Word
data AStar = AStar [Word] Concatenation
instance Show AStar where
    show (AStar word concatenation) = "AStar : words " ++ wordstr ++ ", ... concatination " ++ concstr ++ "..."
        where
            wordstr = foldC $ map show' word
            concstr = foldC (map concToStr $ Map.toList concatenation)

concToStr :: ((Word, Word), Word) -> String
concToStr c@((a, b), d) = (show' a) ++ " * " ++ (show' b) ++ " = " ++ d

data Homomorphism = Homomorphism (Map.Map String String)
instance Show Homomorphism where
    show (Homomorphism h) = "Homomprphism : " ++ strmaps
        where
            strmaps = foldC clist
            clist = map (\t@(a,b) -> (show' a) ++ " -> " ++ (show' b)) $ Map.toList h

data Function = Function (Map.Map String String)
instance Show Function where
    show (Function f) = "Function : " ++ strmaps
        where
            strmaps = foldC clist
            clist = map (\t@(a,b) -> (show' a) ++ " -> " ++ (show' b)) $ Map.toList f

show' :: String -> String
show' "" = "\"\""
show' str = str

sandwich :: String -> [String] -> String
sandwich str [] = ""
sandwich str (x:xs) = foldl (\x y -> x ++ str ++ y) x xs

foldC :: [String] -> String
foldC [] = ""
foldC xs = sandwich ", " xs

generate :: Alphabet -> AStar
generate (Alphabet al) = AStar words concats
    where
        words = aStarWords al
        concats = aStarConcats words

fromList :: [String] -> AStar
fromList xs = generate $ Alphabet $ Set.fromList xs

showAStar :: AStar -> IO()
showAStar (AStar word conc) = do
    putStrLn "Alphabet"
    putStrLn $ (foldC $ map show' word) ++ " ..."
    putStrLn "Concatenations"
    mapM_ (\g -> putStrLn $ (foldC g) ++ " ...") groupedWords
        where
            groupedWords = map (\a -> map concToStr a) $ makeGroup $ Map.toList conc
            makeGroup xs = List.groupBy (\((c, _), _) ((d, _), _) -> c == d) xs

insertion :: Alphabet -> Alphabet -> Maybe Function
insertion (Alphabet a) (Alphabet b) = if (a `Set.isSubsetOf` b)
    then Just $ Function $ Map.fromList $ zip xs xs
    else Nothing
        where xs = Set.toList a

underlyingSet :: AStar -> Alphabet
underlyingSet (AStar words concatination) = Alphabet $ Set.fromList words

underlyingFunction :: Homomorphism -> Function
underlyingFunction (Homomorphism h) = Function h

applyHomomorphism :: [Word] -> Homomorphism -> Maybe Word
applyHomomorphism xs (Homomorphism h) = let 
    conc = List.foldl (++) "" xs
    v = Map.lookup conc h
        in
            case v of
                Just value -> Just (before ++ " = " ++ after ++ " = " ++ value)
                    where
                        before = "f(" ++ conc ++ ")"
                        after = sandwich " * " fx
                        conc = sandwich " * " showed
                        fx = map (\x -> "f(" ++ x ++ ")") showed
                        showed = map show' xs
                Nothing -> Nothing

applyFunction :: Word -> Function -> Maybe Word
applyFunction w (Function f) = Map.lookup w f

compose :: Function -> Function -> Maybe Function
compose (Function g) (Function f) = if (composable (Function g) (Function f))
    then Just $ Function $ Map.fromList $ zip cod dom
    else Nothing
        where cod = List.map (\t -> fst t) $ Map.toList f
              dom = List.map applyFunction $ Map.toList f
              applyFunction t = toValue (Map.lookup (snd t) g)
              toValue m = case m of
                  Just a -> a
                  Nothing -> undefined

composable :: Function -> Function -> Bool
composable (Function g) (Function f) = fcod `Set.isSubsetOf` gdom
    where
        fcod = Set.fromList $ map (\t -> snd t) $ Map.toList f
        gdom = Set.fromList $ map (\t -> fst t) $ Map.toList g

aStarWords :: (Set.Set String) -> [Word]
aStarWords xs = map (\str -> str) $ conc 7 (Set.toList xs)

conc :: Int -> [String] -> [String]
conc n gen = conci [""]
    where
        conci xs = if (n < length xs)
            then xs
            else conci $ xs ++ [i | i <- (allConc xs gen), notElem i xs]

allConc :: [String] -> [String] -> [String]
allConc xs gen = [c ++ str | str <- xs, c <- gen]

aStarConcats :: [Word] -> Concatenation
aStarConcats objs = Map.fromList $ map (\t@(a,b) -> (t, a ++ b)) ot
    where ot = [(a,b) | a <- objs, b <- objs]

ump :: Alphabet -> AStar -> Function -> Maybe Homomorphism
ump (Alphabet al) (AStar word conc) (Function f) = if (acceptable (Alphabet al) (AStar word conc) (Function f))
    then Just $ Homomorphism h
    else Nothing
        where
            h = Map.fromList $ zip dom cod
            dom = alphabetToList $ underlyingSet $ generate (Alphabet al)
            cod = applyF dom
            applyF xs = map (\s -> toString (map justLookup (makeChars s))) xs
            toString xs = foldl (++) "" xs
            justLookup x = Maybe.fromJust (Map.lookup x f)
            makeChars xs = map (\x -> [x]) xs
            alphabetToList (Alphabet a) = Set.toList a

acceptable :: Alphabet -> AStar -> Function -> Bool
acceptable (Alphabet al) (AStar word conc) (Function f) = isFunctionFromAlphabet && thereExistsInsertion
    where
        isFunctionFromAlphabet = (List.sort $ Set.toList al) == fcod
        fcod = List.sort $ map (\t -> (fst t)) $ Map.toList f
        thereExistsInsertion = case (insertion (Alphabet al) (underlyingSet $ generate (Alphabet al))) of
            Just g -> True
            Nothing -> False