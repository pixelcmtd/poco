{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Cocoa as C
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Options.Applicative
import Types

data Target = Cocoa | XComp deriving (Show, Read)

toOutput :: Target -> CompMap -> T.Text
toOutput Cocoa = C.toPlist "§" . C.toTrie
toOutput XComp = undefined

data CliArgs = CliArgs {target :: Target, infile :: FilePath}

cliArgs :: Parser CliArgs
cliArgs =
  CliArgs
    <$> option auto (long "target" <> short 't' <> metavar "TARGET" <> help "the output format: Cocoa or XComp" <> showDefault <> value Cocoa)
    <*> strArgument (metavar "<input.yaml>")

main :: IO ()
main = execParser opts >>= go
  where
    opts =
      info
        (cliArgs <**> helper)
        ( fullDesc
            <> progDesc "Generates Cocoa/XCompose files from the input YAML file."
            <> header "poco - Generate compose files from portable mappings"
        )
    go (CliArgs {target, infile}) = do
      i <- Y.decodeFileThrow infile :: IO CompMap
      -- print i
      putStr . T.unpack $ toOutput target i
