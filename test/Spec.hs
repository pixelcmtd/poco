{-# LANGUAGE OverloadedStrings #-}
-- Behavioral tests: golden output files plus QuickCheck properties. The two
-- decoders below are independent oracles for the output formats (an
-- old-style-plist subset and a Compose(5) subset), so properties can assert
-- that generated output parses back to the input bindings. The internal trie
-- is only observable through toPlist and is not poked at directly.

module Main (main) where

import Cocoa (toPlist, toTrie)
import Data.Char (chr, isHexDigit)
import Data.List (isInfixOf, sort, sortOn)
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Numeric (readHex)
import Test.QuickCheck
import Types (CompMap (..))
import XComp (toXCompose)

main :: IO ()
main = do
  yamlSpec
  goldenSpec
  qc "cocoa preserves bindings" prop_cocoaRoundtrip
  qc "xcompose preserves bindings" prop_xcompRoundtrip
  qc "yaml accepts prefix-free maps" prop_yamlAccepts
  qc "yaml rejects prefix conflicts" prop_yamlRejectsConflict

qc :: Testable p => String -> p -> IO ()
qc name p = do
  r <- quickCheckWithResult stdArgs {maxSuccess = 300} p
  case r of
    Success {} -> pure ()
    _ -> error (name ++ " failed:\n" ++ output r)

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name expected actual
  | actual == expected = return ()
  | otherwise = error (name ++ "\nexpected:\n" ++ show expected
                    ++ "\ngot:\n" ++ show actual)

-- YAML parsing ----------------------------------------------------------------

yaml :: BS.ByteString -> Either String CompMap
yaml input = either (Left . Y.prettyPrintParseException) Right (Y.decodeEither' input)

expectError :: String -> BS.ByteString -> (String -> Bool) -> IO ()
expectError name input ok = case yaml input of
  Left err | ok err -> pure ()
  Left err -> error (name ++ ": rejected with unexpected message:\n" ++ err)
  Right cm -> error (name ++ ": accepted " ++ show cm)

yamlSpec :: IO ()
yamlSpec = do
  expectError "prefix conflict" "a: foo\nab: bar\n"
    ("\"a\" is a prefix of \"ab\"" `isIn`)
  expectError "prefix conflict, other order" "ab: bar\na: foo\n"
    ("\"a\" is a prefix of \"ab\"" `isIn`)
  case yaml "abc: foo\nabd: bar\n" of
    Left err -> error err
    Right (CompMap ps) -> assertEq "shared prefix accepted" 2 (length ps)
  expectError "non-string replacement" "a: [1, 2]\n" ("must be a string" `isIn`)
  expectError "non-mapping input" "- a\n" ("must be a mapping" `isIn`)
  -- an empty trigger has no keysym to press and produces invalid output in
  -- both targets, so it is rejected at parse time (established behavior)
  expectError "empty trigger" "\"\": \"foo\"\n" ("must not be empty" `isIn`)
  expectError "empty trigger with sibling" "\"\": \"foo\"\na: \"bar\"\n"
    ("must not be empty" `isIn`)
  where isIn = isInfixOf

-- Golden output ----------------------------------------------------------------

goldenSpec :: IO ()
goldenSpec = do
  assertEq "keysym table complete" (33 :: Int) (length keyNames)
  let cm = CompMap [("aa", "foo"), ("ab", "bar"), ("cb", "baz")]
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

  assertEq "empty map plist" (Just ("§", [])) (parsePlist (toPlist "§" (toTrie (CompMap []))))
  assertEq "empty map xcompose" (Just []) (parseCompose (toXCompose (CompMap [])))

  -- XCompose: one line per binding, no nesting
  assertEq "xcompose output"
    (T.unlines
      [ "<a> <a> : \"foo\""
      , "<a> <b> : \"bar\""
      , "<c> <b> : \"baz\"" ])
    (toXCompose cm)

  -- XCompose: symbol triggers map to keysym names, space in the middle works,
  -- non-ASCII triggers become zero-padded Unicode keysyms
  assertEq "xcompose symbols"
    (T.unlines
      [ "<a> <grave> : \"ᴀ\""
      , "<exclam> <exclam> : \"‼\""
      , "<bracketleft> <space> <bracketright> : \"☐\""
      , "<underscore> <1> : \"₁\""
      , "<period> <period> : \"…\""
      , "<2> <period> : \"‥\""
      , "<U2026> <a> : \"x\""
      , "<U00D7> <b> : \"z\""
      , "<h> <u> <g> : \"🫂\"" ])
    (toXCompose (CompMap [("a`", "ᴀ"), ("!!", "‼"), ("[ ]", "☐"), ("_1", "₁"),
                          ("..", "…"), ("2.", "‥"), ("…a", "x"), ("×b", "z"), ("hug", "🫂")]))

  ex@(CompMap ps) <- Y.decodeFileThrow "example.yaml" :: IO CompMap
  example <- TIO.readFile "example.dict"
  let expected = Just ("§", sort ps)
      bindings = fmap (\(root, entries) -> (root, sort entries)) . parsePlist
  assertEq "example.dict bindings" expected (bindings example)
  assertEq "generated Cocoa bindings" expected (bindings (toPlist "§" (toTrie ex)))
  exampleX <- TIO.readFile "example.compose"
  assertEq "example.compose lines" (T.lines exampleX) (T.lines (toXCompose ex))

pQuoted :: Text -> Maybe (Text, Text)
pQuoted t0 = case T.uncons (T.stripStart t0) of
  Just ('"', rest) -> go rest ""
  _ -> Nothing
  where
    go t acc = case T.uncons t of
      Just ('"', rest) -> Just (acc, rest)
      Just ('\\', rest) -> case T.uncons rest of
        Just (c, rest') -> go rest' (acc <> T.singleton c)
        Nothing -> Nothing
      Just (c, rest) -> go rest (acc <> T.singleton c)
      Nothing -> Nothing

-- Oracle: old-style plist subset emitted by toPlist ----------------------------

parsePlist :: Text -> Maybe (Text, [(Text, Text)])
parsePlist t0 = do
  t1 <- pChar '{' t0
  (rootKey, t2) <- pQuoted t1
  t3 <- pChar '=' t2
  t4 <- pChar '{' t3
  (bs, t5) <- pBindings t4
  t6 <- pChar ';' t5
  t7 <- pChar '}' t6
  if T.null (T.strip t7) then Just (rootKey, bs) else Nothing

pBindings :: Text -> Maybe ([(Text, Text)], Text)
pBindings = go []
  where
    go acc t = case T.uncons (T.stripStart t) of
      Just ('}', rest) -> Just (concat (reverse acc), rest)
      _ -> do
        (k, t1) <- pQuoted t
        t2 <- pChar '=' t1
        (entry, t3) <- pValue k t2
        go (entry : acc) t3
    pValue k t = case T.uncons (T.stripStart t) of
      Just ('(', t1) -> do
        t2 <- pLit "\"insertText:\", " t1
        (v, t3) <- pQuoted t2
        t4 <- pChar ')' t3
        t5 <- pChar ';' t4
        Just ([(k, v)], t5)
      Just ('{', t1) -> do
        (bs, t2) <- pBindings t1
        t3 <- pChar ';' t2
        Just ([(k <> k', v) | (k', v) <- bs], t3)
      _ -> Nothing

pChar :: Char -> Text -> Maybe Text
pChar c = pLit (T.singleton c)

pLit :: Text -> Text -> Maybe Text
pLit want = T.stripPrefix want . T.stripStart

-- Oracle: Compose(5) subset emitted by toXCompose ------------------------------

parseCompose :: Text -> Maybe [(Text, Text)]
parseCompose = mapM line . T.lines
  where
    line l
      | Just rest <- T.stripPrefix " : " r0 = do
          ks <- mapM token (T.words seqPart)
          trigger <- T.concat <$> mapM fromKeysym ks
          (v, r1) <- pQuoted rest
          if T.null (T.stripStart r1) then Just (trigger, v) else Nothing
      | otherwise = Nothing
      where
        (seqPart, r0) = T.breakOn " : " l
    token t = do
      inner <- T.stripPrefix "<" t
      T.stripSuffix ">" inner

-- the 33 ASCII keysyms poco maps by name, in ASCII order; drift in XComp's
-- table makes these properties fail loudly
keyNames :: [(Text, Char)]
keyNames =
  zip (map T.pack (words "space exclam quotedbl numbersign dollar percent ampersand apostrophe \
             \parenleft parenright asterisk plus comma minus period slash colon \
             \semicolon less equal greater question at bracketleft backslash \
             \bracketright asciicircum underscore grave braceleft bar braceright \
             \asciitilde"))
      " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

fromKeysym :: Text -> Maybe Text
fromKeysym ks
  | T.length ks == 1 = Just ks
  | Just c <- lookup ks keyNames = Just (T.singleton c)
  | Just hex <- T.stripPrefix "U" ks
  , T.length hex >= 4
  , all isHexDigit (T.unpack hex)
  , [(n, "")] <- readHex (T.unpack hex) = Just (T.singleton (chr n))
  | otherwise = Nothing

-- Generators and properties -----------------------------------------------------

-- printable ASCII plus common non-ASCII replacements. Newline is excluded
-- because neither target format escapes it (quote only handles " and \);
-- multiline replacements are out of scope for both formats.
plainChars :: String
plainChars = [' '..'~'] ++ "äöüßÄÖÜ€×…🫂☑₁₂"

-- a mapping whose triggers are prefix-free, as required by Types
newtype ValidMap = ValidMap CompMap deriving (Show, Eq)

instance Arbitrary ValidMap where
  arbitrary = do
    n <- choose (1, 6 :: Int)
    ks <- vectorOf (2 * n) genTrigger
    vs <- vectorOf (length ks) genValue
    pure (ValidMap (CompMap (prune (zip ks vs))))
  shrink (ValidMap (CompMap ps)) =
    [ValidMap (CompMap ps') | ps' <- shrinkList (const []) ps]

genTrigger :: Gen Text
genTrigger = T.pack <$> listOf1 (elements plainChars)

genValue :: Gen Text
genValue = T.pack <$> listOf (elements plainChars)

prune :: [(Text, Text)] -> [(Text, Text)]
prune = foldr keep []
  where
    keep kv@(k, _) acc
      | any (\k' -> k `T.isPrefixOf` k' || k' `T.isPrefixOf` k) (map fst acc) = acc
      | otherwise = kv : acc

prop_cocoaRoundtrip :: ValidMap -> Property
prop_cocoaRoundtrip (ValidMap (CompMap ps)) = case parsePlist (toPlist "§" (toTrie (CompMap ps))) of
  Just ("§", bs) -> sortOn fst bs === sortOn fst ps
  r -> counterexample (show r) False

-- toXCompose emits one line per binding, in input order
prop_xcompRoundtrip :: ValidMap -> Property
prop_xcompRoundtrip (ValidMap (CompMap ps)) =
  parseCompose (toXCompose (CompMap ps)) === Just ps

-- plain lowercase keys/values render as unquoted YAML, so the input file can
-- be built directly; FromJSON does not promise an order, compare as a set
prop_yamlAccepts :: Property
prop_yamlAccepts = forAllShrink gen shr $ \ps ->
  if null ps then discard else case yaml (encodeUtf8 (render ps)) of
    Right (CompMap ps') -> sort ps' === sort ps
    Left e -> counterexample e False
  where
    gen = do
      n <- choose (1, 6 :: Int)
      ks <- vectorOf (2 * n) (T.pack <$> listOf1 (elements ['a' .. 'z']))
      pure (prune (zip ks [T.pack ("v" <> show i) | i <- [1 :: Int ..]]))
    shr = shrinkList (const [])
    render ps = T.concat [k <> ": " <> v <> "\n" | (k, v) <- ps]

prop_yamlRejectsConflict :: Property
prop_yamlRejectsConflict = forAll (suchThat (listOf1 (elements ['a' .. 'z'])) ((>= 2) . length)) $ \k ->
  forAll (choose (1, length k - 1)) $ \i ->
    let p = T.take i (T.pack k)
        input = encodeUtf8 (p <> ": \"x\"\n" <> T.pack k <> ": \"y\"\n")
    in case yaml input of
      Left e -> counterexample e (isInfixOf "is a prefix of" e)
      Right cm -> counterexample ("accepted " ++ show cm) False
