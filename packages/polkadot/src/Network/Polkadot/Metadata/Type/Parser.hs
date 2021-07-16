{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Metadata.Type.Parser
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- This module parse and cleanup raw types given from runtime,
-- drop traits, generics, etc.
--

module Network.Polkadot.Metadata.Type.Parser
  ( fromText
  , fromTextM
  , toText
  , sanitize
  , sanitizeM
  ) where

import           Data.Text                                        (Text,
                                                                   intercalate,
                                                                   pack,
                                                                   replace,
                                                                   strip)
import           Text.Parsec                                      (ParseError,
                                                                   parse)

import           Network.Polkadot.Metadata.Type.Ast
import           Network.Polkadot.Metadata.Type.ParserCombinators (type')

allowed_boxes :: [Text]
allowed_boxes =
  [ "BTreeMap"
  , "BTreeSet"
  , "Compact"
  , "DoNotConstruct"
  , "HashMap"
  , "Int"
  , "Linkage"
  , "Result"
  , "Option"
  , "UInt"
  , "Vec"
  ]

render_box :: Text -> Maybe [TypeAst] -> Text
render_box name Nothing = name
render_box name (Just args)
  | any (== name) allowed_boxes = name <> "<" <> intercalate "," (toText <$> args) <> ">"
  | name == "Box" = toText (head args)
  | otherwise = name

aliases :: Maybe QSelf -> PathSegment -> Text -> Text
aliases _ _ "Vec<u8>"            = "Bytes"
aliases _ _ "BoundedVec"         = "Vec"
aliases _ _ "Announcement"       = "ProxyAnnouncement"
aliases _ _ "Status"             = "BalanceStatus"
aliases (Just (q, _)) _ "Source" = toText q <> "Source"
aliases (Just (q, _)) _ "Target" = toText q <> "Target"
aliases _ _ a                    = a

-- | Render Metadata type to text.
--
-- This function strongly sanitize type identifiers and paths,
-- removes generics and other Rust related staff.
toText :: TypeAst -> Text
toText (Slice (Path Nothing [("u8", Nothing)])) = "Bytes"
toText (Slice t) = "[" <> toText t <> "]"
toText (Tuple [t]) = toText t
toText (Tuple ts)  = "(" <> intercalate "," (toText <$> ts) <> ")"
toText (Array t n) = "[" <> toText t <> ";" <> pack (show n) <> "]"
toText (Path _ []) = "()"
toText (Path q xs) = aliases q (last xs) $ render_box name args
  where name = fst (last xs)
        args = snd (last xs)

-- | Parse metadata type (general Rust type) from text.
fromText :: Text -> Either ParseError TypeAst
fromText = parse type' "Metadata Type"
         . strip
         . replace "\n" ""
         . replace "\n " ""

-- | This variant of `fromText` fails when error happens.
fromTextM :: MonadFail m => Text -> m TypeAst
fromTextM t = either (fail . ((show t ++ ": ") ++) . show) return $ fromText t

-- | Cleanup type or return error when syntax failure.
sanitize :: Text -> Either ParseError Text
{-# INLINE sanitize #-}
sanitize = fmap toText . fromText

-- | Cleanup type or throw fail call when syntax failure.
sanitizeM :: MonadFail m => Text -> m Text
{-# INLINE sanitizeM #-}
sanitizeM = fmap toText . fromTextM
