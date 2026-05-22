module DecisionTree where

-- Decision tree implementation with probaiblistic labels
-- and entropy-based attribute selection

-- Copyright Dominic Orchard 2025

import Data.List (partition)

----------------
-- # Key data types

type Attribute       = String
type Value           = String
type Row             = [Value]
type Header          = [Attribute]

data Label           = Yes | No
  deriving (Show, Eq)

type TrainingDataSet = (Header, [(Row, Label)])

-- Key decision tree type
data DecisionTree =
    Leaf (Label, Float) -- Label with probability (confidence)
    -- ^ A leaf node with a Label value
  | Node Attribute [(Value, DecisionTree)]
    -- ^ A node with an attribute and a list of branches
  deriving Show

-- A partition is a value a list of values paired with a training data set
-- (which does not contain that value or any data belong to that attribute)
type Partition = [(Value, TrainingDataSet)]

-- Add a row to a training data set
addRow :: TrainingDataSet -> (Row, Label) -> TrainingDataSet
addRow (header, rows) (row, label) = (header, (row, label) : rows)

-- # Helper functions

lookupAttribute :: Header -> Row -> Attribute -> Maybe Value
lookupAttribute (h:hs) (r:rs) attr =
  if h == attr
    then Just r
    else lookupAttribute hs rs attr
lookupAttribute _ _ _ = Nothing

removeAttribute :: Header -> Row -> Attribute -> Row
removeAttribute (h:hs) (r:rs) attr =
  if h == attr
    then rs
    else
     r : removeAttribute hs rs attr
removeAttribute _ r _ = r

removeFromHeader :: Attribute -> Header -> Header
removeFromHeader _ [] = []
removeFromHeader x (y:rest)
  | x == y = removeFromHeader x rest
  | otherwise = y : removeFromHeader x rest

allSameLabel :: [(Row, Label)] -> Maybe Label
allSameLabel [] = Nothing
allSameLabel ((r, v):rss) =
  if all (\(_, v') -> v == v') rss
    then Just v
    else Nothing

nodes :: DecisionTree -> Int
nodes (Leaf _) = 1
nodes (Node _ branches) = 1 + sum (map (nodes . snd) branches)

-----------------------

lookupBranch :: Value -> [(Value, DecisionTree)] -> Maybe DecisionTree
lookupBranch _ [] = Nothing
lookupBranch val ((v, tree):rest) =
  if val == v
    then Just tree
    else lookupBranch val rest

-- Infer a label from the decision tree alongisde a probability (confidence
-- value) for that label.
infer :: DecisionTree -> Header -> Row -> Maybe (Label, Float)
infer (Leaf (val, p)) _ _ = Just (val, p)
infer (Node attr branches) hs rs =
  case lookupAttribute hs rs attr of
    Nothing -> Nothing
    Just val ->
      case lookupBranch val branches of
        Nothing -> Nothing
        Just tree -> infer tree hs rs

validateTree :: DecisionTree -> TrainingDataSet -> Float
validateTree tree (_, []) = 1
validateTree tree (h, (row,l):rest) =
  case infer tree h row of
    Nothing -> 0
    Just (l', p) | l == l' -> p * validateTree tree (h, rest)
                 | otherwise -> 0.0


addToPartition :: Value -> (Row, Label) -> Header -> Partition -> Partition
addToPartition val lrow h [] = [(val, (h, [lrow]))]
addToPartition val lrow h ((val', (h', rs)):p) =
  if val == val'
    then (val', (h', lrow : rs)) : p
    else (val', (h', rs)) : addToPartition val lrow h p

-- partitionData takes a training data set and an attribute and returns a partition
-- of the data set based on the values of the attribute
partitionData :: TrainingDataSet -> Attribute -> Partition
partitionData (_, []) _ = []
partitionData (hs, (rs, outcome):rss) attr =
  case lookupAttribute hs rs attr of
    Just val ->
      addToPartition val (removeAttribute hs rs attr, outcome) hs'
        (partitionData (hs, rss) attr)
      where
        hs' = removeFromHeader attr hs
    Nothing -> partitionData (hs, rss) attr

-- Learn a decision tree from a data set given an attribute selector
-- Note that this version does not assume that the training data set enumerates
-- all possibilities. Instead, it is able to handle a situation where there is
-- only partial knowledge and therefore construct a leaf node with a label
-- and a probability of seeing that label.
learnTree :: TrainingDataSet -> (TrainingDataSet -> Attribute) -> DecisionTree
learnTree (_, []) _ = Leaf (No, 0)
learnTree ([], rows) _ =
    -- Most common outcome from the rows
      if length yes >= length no
        then Leaf (Yes, yn / (yn + nn))
        else Leaf (No, nn / (yn + nn))
    where
      yn = fromInteger . toInteger $ length yes
      nn = fromInteger . toInteger $ length no
      (yes, no) = partition (\r -> snd r == Yes) rows

learnTree ds chooseAttr =
    case allSameLabel (snd ds) of
      Nothing ->
        Node attr (map (\(val, ds') -> (val, learnTree ds' chooseAttr)) partition)
          where
            partition = partitionData ds attr
            attr = chooseAttr ds
      Just o  -> Leaf (o, 1)
  where
    (header, rows) = ds

-- Handy function
xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p
  where
    log2 x = log x / log 2

{-
ghci> entropyLabel trainingDataSet
0.9402859586706309
-}
entropyLabel :: TrainingDataSet -> Double
entropyLabel (header, rows) =
    - (xlogx (yesCount / n)) - (xlogx (noCount / n))
  where
    n        = fromIntegral (length rows)
    yesCount = fromIntegral (length (filter (== Yes) $ map snd rows))
    noCount  = fromIntegral (length (filter (== No) $ map snd rows))

{-
--Information gain
ghci> gain "outlook" trainingDataSet
0.2467498197744391
-}
gain :: Attribute -> TrainingDataSet -> Double
gain attr dataSet =
      (entropyLabel dataSet) - (sum $ map local partition)
  where
    n = fromIntegral (length (snd dataSet))
    local (_, pset) = (fromIntegral (length (snd pset)) / n) * (entropyLabel pset)
    partition = partitionData dataSet attr

{-
ghci> bestGain trainingDataSet
"outlook"
-}
bestGain :: TrainingDataSet -> Attribute
bestGain (header, rows) =
    fst $ findMax (map (\attr -> (attr, gain attr (header, rows))) header)
  where
    findMax [(name, gain)] = (name, gain)
    findMax ((name, gain):rest) =
      case findMax rest of
        (name', gain') ->
           if gain > gain'
            then (name, gain)
            else (name', gain')
