{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lib2.ArrayFunc (
    getByExtractorFromArray,
    getByIndexFromArray,
    getIndexFromArray,
    addToArray,
    updateArray,
    deleteFromArray,
    elementInArray
) where

    
getByExtractorFromArray :: Eq a => a -> (b -> a) -> [b] -> Either String b
getByExtractorFromArray = getByIndexFromArray'
  where
    getByIndexFromArray' _ _ [] = Left "Element not found"
    getByIndexFromArray' value' extractor' (h:t) =
      if value' == extractor' h
      then Right h
      else getByIndexFromArray' value' extractor' t

getByIndexFromArray :: Eq a => Int -> [a] -> Either String a
getByIndexFromArray index arr = getByIndexFromArray' index arr 0
  where
    getByIndexFromArray' _ [] _ = Left "Element not found"
    getByIndexFromArray' index' (h:t) i = if index' == i then Right h else getByIndexFromArray' index' t (i + 1)

getIndexFromArray :: Eq a => a -> [a] -> Either String Int
getIndexFromArray target arr = getIndexFromArray' target arr 0
  where
    getIndexFromArray' _ [] _ = Left "Element not found"
    getIndexFromArray' target' (h:t) index = if target' == h then Right index else getIndexFromArray' target' t (index + 1)

addToArray :: Eq a => a -> [a] -> Either String [a]
addToArray = addToArray' []
  where
    addToArray' acc target [] = Right (acc ++ [target])
    addToArray' acc target (h:t) = if target == h then Left "Element already exists" else Right (acc ++ [h] ++ t)

updateArray :: Eq a => a -> a -> [a] ->  Either String [a]
updateArray = updateArray' []
  where
    updateArray' _ _ _ [] = Left "Element not found"
    updateArray' acc target new (h:t) = if target == h then Right (acc ++ [new] ++ t) else updateArray' (acc ++ [h]) target new t

deleteFromArray :: Eq a => a -> [a] -> Either String [a]
deleteFromArray = deleteFromArray' []
  where
    deleteFromArray' _ _ [] = Left "Element not found"
    deleteFromArray' acc target (h:t) = if target == h then Right (acc ++ t) else deleteFromArray' (acc ++ [h]) target t

elementInArray :: Eq a => a -> [a] -> Bool
elementInArray _ [] = False
elementInArray target (h:t) = if target == h then True else elementInArray target t


