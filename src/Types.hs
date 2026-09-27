module Types (CompMap (..), quote) where

import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y

newtype CompMap = CompMap [(Text, Text)] deriving (Eq, Show)

instance Y.FromJSON CompMap where
  parseJSON (Y.Object o) = do
    ps <- mapM toPair (toList o)
    let keys = sort (map fst ps)
    case [(a, b) | (a, b) <- zip keys (drop 1 keys), a `T.isPrefixOf` b] of
      (a, b) : _ -> fail ("Conflicting trigger sequences: " ++ show a ++ " is a prefix of " ++ show b)
      [] -> pure (CompMap ps)
    where
      toPair (k, Y.String s)
        | T.null (toText k) = fail "trigger sequence must not be empty"
        | otherwise = pure (toText k, s)
      toPair (k, invalid) =
        prependFailure
          ("replacement for trigger \"" <> T.unpack (toText k) <> "\" must be a string ")
          (typeMismatch "String" invalid)
  parseJSON invalid =
    prependFailure
      "input must be a mapping from trigger strings to replacement strings "
      (typeMismatch "Object" invalid)

quote :: Text -> Text
quote s = T.singleton '"' <> T.concatMap esc s <> T.singleton '"'
  where
    esc '"'  = T.pack ['\\', '"']
    esc '\\' = T.pack ['\\', '\\']
    esc c    = T.singleton c
