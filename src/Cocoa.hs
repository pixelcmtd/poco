{-# LANGUAGE OverloadedStrings #-}

-- we make the untested assumption that no binding is the prefix of another binding
-- FIXME: there should be a test for this somewhere else
module Cocoa (Mark (..), toTrie, toPlist, alg, coalg) where

import Data.Functor.Base (TreeF (NodeF))
import Data.Functor.Foldable (ana, cata)
import Data.List (nub, partition)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree)
import Types (CompMap (..))

data Mark = Root | Edge Char | Value Text deriving (Show, Eq)

toTrie :: CompMap -> Tree Mark
toTrie cm = ana coalg (Root, cm)

coalg :: (Mark, CompMap) -> TreeF Mark (Mark, CompMap)
coalg (m, CompMap []) = NodeF m []
coalg (m, CompMap ps) =
  NodeF
    m
    ( [(Value v, CompMap []) | (_, v) <- done]
        ++ [(Edge (T.head k), CompMap [(T.tail k', v) | (k', v) <- g]) | g <- grouped, (k, _) <- take 1 g]
    )
  where
    (done, rest) = partition (T.null . fst) ps
    grouped = [[(k, v) | (k, v) <- rest, T.head k == c] | c <- nub (map (T.head . fst) rest)]

toPlist :: Text -> Tree Mark -> Text
toPlist rootKey tree = T.unlines . snd $ cata (alg rootKey) tree

alg :: Text -> TreeF Mark (Mark, [Text]) -> (Mark, [Text])
alg _ (NodeF (Value v) []) = (Value v, ["(\"insertText:\", " <> quote v <> ")"])
alg _ (NodeF (Edge k) [(Value _, [v])]) = (Edge k, [T.singleton k `eq` (v <> ";")])
alg k (NodeF m cs) = case m of
  Root -> (Root, "{" <> k `eq` "{" : linesIn cs ++ ["};}"])
  (Edge key) -> (Edge key, T.singleton key `eq` "{" : linesIn cs ++ ["};"])
  _ -> undefined
  where
    linesIn = concatMap (map ("  " <>) . snd)

eq :: Text -> Text -> Text
eq k rest = quote k <> " = " <> rest

quote :: Text -> Text
quote s = "\"" <> T.concatMap esc s <> "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c = T.singleton c
