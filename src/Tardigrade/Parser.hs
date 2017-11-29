{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Tardigrade.Parser
    ( parse
    , Program(..)
    ) where

import           Control.Applicative   ((<|>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec       (Dec, ParseError, Token, sepBy)
import qualified Text.Megaparsec       as MP
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)


-- XSH would call this a `program` and that makes more semantic sense than a 'command'
-- especially when the context is that of a script being interpreted.
data Program where
  Command :: Text -> [Text] -> Program
  deriving (Show)

-- TODO This is a parser _and_ a lexer at the moment.
-- Which is uncool; we want to separate concerns and
-- feed a parser a stream of tokens (which will make
-- things a lot simpler to debug).
-- TODO This is actually just the parser for `Command'
-- so best if we clarify that later via its name.
-- TODO At the moment this will include quotes for
-- tokens e.g. `-g'Main*'` whereas most shells will
-- parse that result into `-gMain*`, indicating
-- that the expansion takes place (whether preserving
-- literal content with single quotes or interpolating
-- with double quotes) and then strips the quotes from
-- the token itself. So, perhaps it's best this happens
-- at an 'expansion' stage, if we want that.
program :: Parser Program
program = do
    MP.space
    cmd <- MP.some MP.alphaNumChar
    MP.space
    args <- MP.some flagOrOption `MP.sepEndBy` whitespace
    return $ Command (T.pack cmd) (fmap T.pack args)
  where
    flagOrOption = MP.try MP.alphaNumChar <|> MP.punctuationChar
    whitespace   = MP.some MP.spaceChar

{- parse :: Text -> Either (ParseError (Token Text) Dec) Program -}
parse :: Text -> Either String Program
parse line = case MP.parse program "" line of
               Left _    -> Left $ "Unable to parse input."
               Right prg -> Right prg
