{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Network.Ethereum.Web3.TH (abi, abiFrom) where

import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy          as LT
import qualified Data.Text               as T
import Network.Ethereum.Web3.Internal
import Network.Ethereum.Web3.Contract
import Network.Ethereum.Web3.JsonAbi
import Network.Ethereum.Web3.Types
import Data.Text (Text, isPrefixOf)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH
import Data.Aeson

abiFrom :: QuasiQuoter
abiFrom = quoteFile abi

abi :: QuasiQuoter
abi = QuasiQuoter
  { quoteDec  = quoteAbiDec
  , quoteExp  = quoteAbiExp
  , quotePat  = undefined
  , quoteType = undefined
  }

-- | Instance declaration with empty context
instanceD' :: Name -> TypeQ -> [DecQ] -> DecQ
instanceD' name insType insDecs =
    instanceD (cxt []) (appT insType (conT name)) insDecs

-- | Simple data type declaration with one constructor
dataD' :: Name -> ConQ -> [Name] -> DecQ
dataD' name rec derive =
    dataD (cxt []) name [] Nothing [rec] $ cxt (conT <$> derive)

-- | Simple function declaration
funD' :: String -> [PatQ] -> ExpQ -> DecQ
funD' name p f = funD (mkName name) [clause p (normalB f) []]

typeQ :: Text -> TypeQ
typeQ typ | T.any (== '[') typ = appT listT (go (T.takeWhile (/= '[') typ))
          | otherwise          = go typ
  where go x | "string"  == x         = conT (mkName "Text")
             | "address" == x         = conT (mkName "Address")
             | "bytes"   == x         = conT (mkName "BytesD")
             | "bool"    == x         = conT (mkName "Bool")
             | "bytes" `isPrefixOf` x = appT (conT (mkName "BytesN"))
                                             (numLit (T.drop 5 x))
             | "int"   `isPrefixOf` x = conT (mkName "Integer")
             | "uint"  `isPrefixOf` x = conT (mkName "Integer")
             | otherwise = fail ("Unknown type: " ++ T.unpack x)
        numLit n = litT (numTyLit (read (T.unpack n)))

eventBangType :: EventArg -> BangTypeQ
eventBangType (EventArg _ typ _) =
    bangType (bang sourceNoUnpack sourceStrict) (typeQ typ)

eventEncodigFuns :: Name -> [EventArg] -> [DecQ]
eventEncodigFuns eventName args = [ funD' "toDataBuilder"  [] toDataB
                                  , funD' "fromDataParser" [] fromDataP ]
  where toDataB    = [|error "Event to data conversion isn't available!"|]
        indexed    = map eveArgIndexed args
        parseArg v = bindS (varP v) [|fromDataParser|]
        genVar x   = do v <- newName "t"
                        return (x, v)
        fromDataP  = do
            vars <- mapM genVar indexed
            let indexedVars   = fmap snd (filter fst vars)
                unindexedVars = fmap snd (filter (not . fst) vars)
            doE $ fmap parseArg indexedVars
               ++ fmap parseArg unindexedVars
               ++ [noBindS [|return $(appsE (conE eventName : fmap (varE . snd) vars))|]]

eventFilterD :: String -> [DecQ]
eventFilterD topic0 = let addr = mkName "a" in
  [ funD' "eventFilter" [wildP, varP addr]
    [|Filter (Just $(varE addr))
             (Just [Just topic0, Nothing])
             Nothing
             Nothing
     |]
  ]

-- | Event declarations maker
mkEvent :: Declaration -> Q [Dec]
mkEvent eve@(DEvent name inputs _) = sequence $
    [ dataD' eventName eventFields derivingD
    , instanceD' eventName encodingT (eventEncodigFuns eventName inputs)
    , instanceD' eventName eventT    (eventFilterD (T.unpack $ eventId eve))
    ]
  where eventName   = mkName (toUpperFirst (T.unpack name))
        derivingD   = [mkName "Show", mkName "Eq", mkName "Ord"]
        eventFields = normalC eventName (eventBangType <$> inputs)
        encodingT   = conT (mkName "ABIEncoding")
        eventT      = conT (mkName "Event")

-- | Method delcarations maker
mkFun :: Declaration -> Q [Dec]
mkFun _ = undefined

-- | Declaration parser
mkDecl :: Declaration -> Q [Dec]
mkDecl x@(DEvent _ _ _) = mkEvent x
mkDecl x@(DFunction _ _ _) = return []  --mkFun x
mkDecl _ = return []

-- | ABI to declarations converter
quoteAbiDec :: String -> Q [Dec]
quoteAbiDec abi_string =
    case decode abi_lbs of
        Just (ContractABI abi) -> concat <$> mapM mkDecl abi
        _ -> fail "Unable to parse ABI!"
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)

-- | ABI information string
quoteAbiExp :: String -> ExpQ
quoteAbiExp abi_string = stringE $
    case eitherDecode abi_lbs of
        Left e    -> "Error: " ++ show e
        Right abi -> show (abi :: ContractABI)
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)
