{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE CPP             #-}
-- |
-- Module      :  Network.Ethereum.Web3.TH
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- TemplateHaskell based Ethereum contract ABI
-- methods & event generator for Haskell native API.
--
-- @
-- [abiFrom|data/sample.json|]
--
-- main = do
--     runWeb3 $ event "0x..." $
--        \(Action2 n x) -> do print n
--                             print x
--     wait
--   where wait = threadDelay 1000000 >> wait
-- @
--
module Network.Ethereum.Web3.TH (abi, abiFrom) where

import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Builder  as B
import qualified Data.Text.Lazy          as LT
import qualified Data.Attoparsec.Text    as P
import qualified Data.Text               as T
import Network.Ethereum.Web3.Address (Address)
import Network.Ethereum.Web3.Internal
import Network.Ethereum.Web3.Contract
import Network.Ethereum.Web3.JsonAbi
import Network.Ethereum.Web3.Types
import Data.Text (Text, isPrefixOf)
import Data.List (groupBy, sortBy)
import Data.Monoid (mconcat, (<>))
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH
import Control.Arrow
import Data.Aeson

-- | Read contract ABI from file
abiFrom :: QuasiQuoter
abiFrom = quoteFile abi

-- | QQ reader for contract ABI
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
#if MIN_VERSION_template_haskell(2,12,0)
    dataD (cxt []) name [] Nothing [rec] [derivClause Nothing (conT <$> derive)]
#else
    dataD (cxt []) name [] Nothing [rec] $ cxt (conT <$> derive)
#endif

-- | Simple function declaration
funD' :: Name -> [PatQ] -> ExpQ -> DecQ
funD' name p f = funD name [clause p (normalB f) []]

-- | ABI and Haskell types association
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

-- | Event argument to TH type
eventBangType :: EventArg -> BangTypeQ
eventBangType (EventArg _ typ _) =
    bangType (bang sourceNoUnpack sourceStrict) (typeQ typ)

-- | Function argument to TH type
funBangType :: FunctionArg -> BangTypeQ
funBangType (FunctionArg _ typ) =
    bangType (bang sourceNoUnpack sourceStrict) (typeQ typ)

-- | Solidity dynamic type predicate
isDynType :: Text -> Bool
isDynType "bytes"  = True
isDynType "string" = True
isDynType x | T.any (== '[') x = True
            | otherwise        = False

-- | ABI encoding generator
abiEncodingParse :: [(Text, Name)] -> [StmtQ]
abiEncodingParse vars = fmap parseSta vars
                     ++ fmap (parseVar . snd) dynVars
  where dynVars = filter (isDynType . fst) vars
        parseSta (t, v) | isDynType t = noBindS [|P.take 64|]
                        | otherwise   = parseVar v
        parseVar v = bindS (varP v) [|fromDataParser|]

eventEncodigD :: Name -> [EventArg] -> [DecQ]
eventEncodigD eventName args = [ funD' (mkName "toDataBuilder")  [] toDataB
                               , funD' (mkName "fromDataParser") [] fromDataP ]
  where toDataB = [|error "Event to data conversion isn't available!"|]
        indexed = map (eveArgType &&& eveArgIndexed) args
        genVar (a, b)   = do v <- newName "t"
                             return (b, (a, v))
        parseArg (_, v) = bindS (varP v) [|fromDataParser|]
        fromDataP  = do
            vars <- mapM genVar indexed
            let indexedVars   = [v | (ix, v) <- vars, ix]
                unindexedVars = [v | (ix, v) <- vars, not ix]
                freeVars      = [varE v | (_, (_, v)) <- vars]
            doE $ fmap parseArg indexedVars
               ++ abiEncodingParse unindexedVars
               ++ [noBindS [|return $(appsE (conE eventName : freeVars))|]]

genABIHeader :: [(Text, Name)] -> [ExpQ]
genABIHeader vars = fmap go offsetVars
  where offsetVars :: [((Text, Name), Int)]
        offsetVars = zip vars (fmap ((32 *) . (length vars +)) [0..])
        go ((typ, v), o) | isDynType typ = [|toDataBuilder (o :: Int)|]
                         | otherwise     = [|toDataBuilder $(varE v)|]

genABIData :: [(Text, Name)] -> [ExpQ]
genABIData = fmap (\(_, v) -> [|toDataBuilder $(varE v)|])

funEncodigD :: Name -> [FunctionArg] -> String -> [DecQ]
funEncodigD funName args ident =
    [ funDtoDataB
    , funD' (mkName "fromDataParser") [] fromDataP ]
  where fromDataP = [|error "Function from data conversion isn't available!"|]
        funDtoDataB = do
            vars <- sequence $ replicate (length args) (newName "t")
            funD' (mkName "toDataBuilder")
                  [conP funName $ fmap varP vars]
                  (toDataB $ zip argTypes vars)
        argTypes  = fmap funArgType args
        toDataB vars = do
            let dynamicVars = filter (isDynType . fst) vars
            appE [|mconcat|] $
                listE $ [|B.fromText ident|]
                      : genABIHeader vars ++ genABIData dynamicVars

eventFilterD :: String -> [DecQ]
eventFilterD topic0 = let addr = mkName "a" in
  [ funD' (mkName "eventFilter") [wildP, varP addr]
    [|Filter (Just $(varE addr))
             (Just [Just topic0, Nothing])
             Nothing
             Nothing
     |]
  ]

{-
 - TODO
 -
funTypeWrapper :: Name -> [FunctionArg] -> Maybe [FunctionArg] -> DecQ
funTypeWrapper funName args result = sigD funName funType
  where
    funType = foldl appT [t|Address|] $ arrowing (inputT ++ [outputT])
    arrowing= concat . zipWith (\a b -> [a, b]) (repeat arrowT)
    inputT  = fmap (typeQ . funArgType) args
    outputT = case result of
        Nothing  -> [t|Web3 ()|]
        Just [x] -> [t|Web3 $(typeQ $ funArgType x)|]
        Just xs  -> let outs = fmap (typeQ . funArgType) xs
                    in  [t|Web3 $(foldl appT (tupleT (length xs)) outs)|]
-}

funWrapper :: Bool -> Name -> Name -> [FunctionArg] -> DecQ
funWrapper c name dname args = do
    (a : b : vars) <- sequence $ replicate (length args + 2) (newName "t")
    let params = appsE ((conE dname) : fmap varE vars)
    case c of
        True  -> funD' name (fmap varP (a : vars)) $
            [|call $(varE a) Latest $(params)|]
        False -> funD' name (fmap varP (a : b : vars)) $
            [|sendTx $(varE a) $(varE b) $(params)|]

-- | Event declarations maker
mkEvent :: Declaration -> Q [Dec]
mkEvent eve@(DEvent name inputs _) = sequence $
    [ dataD' eventName eventFields derivingD
    , instanceD' eventName encodingT (eventEncodigD eventName inputs)
    , instanceD' eventName eventT    (eventFilterD (T.unpack $ eventId eve))
    ]
  where eventName   = mkName (toUpperFirst (T.unpack name))
        derivingD   = [mkName "Show", mkName "Eq", mkName "Ord"]
        eventFields = normalC eventName (eventBangType <$> inputs)
        encodingT   = conT (mkName "ABIEncoding")
        eventT      = conT (mkName "Event")

-- | Method delcarations maker
mkFun :: Declaration -> Q [Dec]
mkFun fun@(DFunction name constant inputs outputs) = do
    sequence $
        [ dataD' dataName (normalC dataName bangInput) derivingD
        , instanceD' dataName encodingT (funEncodigD dataName inputs mIdent)
        , instanceD' dataName methodT []
        -- , funTypeWrapper funName inputs outputs
        , funWrapper constant funName dataName inputs
        ]
  where mIdent    = T.unpack (methodId fun)
        dataName  = mkName (toUpperFirst (T.unpack $ name <> "Data"))
        funName   = mkName (toLowerFirst (T.unpack name))
        bangInput = fmap funBangType inputs
        derivingD = [mkName "Show", mkName "Eq", mkName "Ord"]
        encodingT = conT (mkName "ABIEncoding")
        methodT   = conT (mkName "Method")

escape :: [Declaration] -> [Declaration]
escape = concat . escapeNames . groupBy fnEq . sortBy fnCompare
  where fnEq (DFunction n1 _ _ _) (DFunction n2 _ _ _) = n1 == n2
        fnEq _ _ = False
        fnCompare (DFunction n1 _ _ _) (DFunction n2 _ _ _) = compare n1 n2
        fnCompare _ _ = GT

escapeNames :: [[Declaration]] -> [[Declaration]]
escapeNames = fmap go
  where go (x : xs) = x : zipWith appendToName xs hats
        hats = [T.replicate n "'" | n <- [1..]]
        appendToName dfn addition = dfn { funName = funName dfn <> addition }

-- | Declaration parser
mkDecl :: Declaration -> Q [Dec]
mkDecl x@(DFunction{}) = mkFun x
mkDecl x@(DEvent{})    = mkEvent x
mkDecl _ = return []

-- | ABI to declarations converter
quoteAbiDec :: String -> Q [Dec]
quoteAbiDec abi_string =
    case decode abi_lbs of
        Just (ContractABI abi) -> concat <$> mapM mkDecl (escape abi)
        _ -> fail "Unable to parse ABI!"
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)

-- | ABI information string
quoteAbiExp :: String -> ExpQ
quoteAbiExp abi_string = stringE $
    case eitherDecode abi_lbs of
        Left e    -> "Error: " ++ show e
        Right abi -> show (abi :: ContractABI)
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)
