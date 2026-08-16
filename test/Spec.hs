{-# LANGUAGE OverloadedStrings #-}
-- TODO: replace this vibecoded mess with quickcheck or whatever

module Main (main) where

import Cocoa (Mark (..), toPlist, toTrie)
import Data.Char (isSpace)
import Data.Tree (Tree (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types (CompMap (..))

cm :: CompMap
cm = CompMap [("aa", "foo"), ("ab", "bar"), ("cb", "baz")]

main :: IO ()
main = do
  let trie = toTrie cm
  assertEq "root label" Root (rootLabel trie)
  assertEq "root has 2 branches" 2 (length (subForest trie))

  let aSub = head (subForest trie)
  assertEq "'a' node" (Edge 'a') (rootLabel aSub)
  assertEq "'a' has 2 children" 2 (length (subForest aSub))

  let aa = head (subForest aSub)
      ab = head (drop 1 (subForest aSub))
  assertEq "'a'->'a' node" (Edge 'a') (rootLabel aa)
  assertEq "'a'->'a' value" [Value "foo"] (map rootLabel (subForest aa))
  assertEq "'a'->'b' node" (Edge 'b') (rootLabel ab)
  assertEq "'a'->'b' value" [Value "bar"] (map rootLabel (subForest ab))

  -- second branch: 'c'
  let cSub = head (drop 1 (subForest trie))
  assertEq "'c' node" (Edge 'c') (rootLabel cSub)
  assertEq "'c' has 1 child" 1 (length (subForest cSub))
  let cb = head (subForest cSub)
  assertEq "'c'->'b' node" (Edge 'b') (rootLabel cb)
  assertEq "'c'->'b' value" [Value "baz"] (map rootLabel (subForest cb))

  -- empty mapping: bare root
  let emptyTrie = toTrie (CompMap [])
  assertEq "empty root label" Root (rootLabel emptyTrie)
  assertEq "empty has no children" [] (subForest emptyTrie)

  -- a key that is a prefix of another: the value sits alongside deeper
  -- branches
  let deep = toTrie (CompMap [("abc", "x"), ("abd", "y")])
  let a2 = head (subForest deep)        -- Edge 'a'
      b2 = head (subForest a2)          -- Edge 'b'
  assertEq "deep 'ab' has 2 children" 2 (length (subForest b2))
  assertEq "['a','b'] children labels" [Edge 'c', Edge 'd']
    (map rootLabel (subForest b2))

  -- serialization: canonical example becomes the expected plist
  assertEq "plist output"
    (T.unlines
      [ "{\"\" = {"
      , "  \"a\" = {"
      , "    \"a\" = (\"insertText:\", \"foo\");"
      , "    \"b\" = (\"insertText:\", \"bar\");"
      , "  };"
      , "  \"c\" = {"
      , "    \"b\" = (\"insertText:\", \"baz\");"
      , "  };"
      , "};}" ])
    (toPlist "" (toTrie cm))

  let allKeys = [ ("hug", "🫂"), ("pnt", "👉👈"), ("pls", "🥺"), ("cdot", "⋅")
        , ("chk", "☑"), ("xx", "×"), ("eu", "€"), ("SS", "ẞ"), ("ss", "ß")
        , ("Ue", "Ü"), ("Oe", "Ö"), ("Ae", "Ä"), ("ue", "ü"), ("oe", "ö")
        , ("ae", "ä"), ("a`", "ᴀ"), ("dgc", "℃"), ("degc", "℃")
        , ("degC", "℃"), ("b`", "ʙ"), ("[x]", "☒"), ("[ ]", "☐")
        , ("_2", "₂"), ("_1", "₁"), ("2.", "‥"), ("..", "…") ]
  example <- TIO.readFile "example.dict"
  assertEq "example.dict tokens" (tokens example)
    (tokens (toPlist "§" (toTrie (CompMap allKeys))))

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
  | actual == expected = return ()
  | otherwise = error (name ++ "\nexpected:\n" ++ show expected
                    ++ "\ngot:\n" ++ show actual)

tokens :: T.Text -> T.Text
tokens = T.filter (not . isSpace)
