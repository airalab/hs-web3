{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Metadata.Type.ParserCombinators
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser combinators for the metadata type.
--

module Network.Polkadot.Metadata.Type.ParserCombinators where

import           Data.Functor.Identity              (Identity)
import           Data.Text                          (pack)
import           Text.Parsec
import           Text.Parsec.Text                   ()

import           Network.Polkadot.Metadata.Type.Ast (PathSegment, QSelf,
                                                     TypeAst (..))

type' :: Stream s Identity Char => Parsec s () TypeAst
type' = choice
  [ Slice <$> try slice
  , Tuple <$> tuple
  , Array <$> arrayT <*> arrayN
  , Path <$> optionMaybe pathQself <*> path
  ]

slice :: Stream s Identity Char => Parsec s () TypeAst
slice = (string "[" <|> string "&[") *> type' <* char ']'

tuple :: Stream s Identity Char => Parsec s () [TypeAst]
tuple = char '(' *> type' `sepBy1` comma <* char ')'

arrayT :: Stream s Identity Char => Parsec s () TypeAst
arrayT = char '[' *> type' <* dotcomma
  where dotcomma = try (string "; ") <|> string ";"

arrayN :: Stream s Identity Char => Parsec s () Int
arrayN = read <$> manyTill digit (char ']')

pathQself :: Stream s Identity Char => Parsec s () QSelf
pathQself = (char '<' *> qselfP <* char '>') <* string "::"
  where qselfP = (,) <$> (type' <* string " as ") <*> type'

path :: Stream s Identity Char => Parsec s () [PathSegment]
path = path' `sepBy` string "::"
  where
    path' = (,) <$> ident
                <*> optionMaybe (char '<' *> type' `sepBy1` comma <* char '>')
    ident = do
        c <- letter
        cs <- many (alphaNum <|> char '_')
        return $ pack (c : cs)

comma :: Stream s Identity Char => Parsec s () String
comma = try (string ", ") <|> string ","
