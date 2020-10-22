{-# LANGUAGE OverloadedStrings #-}
module Crypto.Ethereum.Test.SignatureSpec where

import           Crypto.Ecdsa.Utils        (importKey)
import           Crypto.Ethereum.Signature (hashMessage, signMessage)
import           Data.ByteArray            (convert)
import           Data.ByteArray.HexString  (HexString)
import           Data.Foldable             (for_)
import           Test.Hspec

test_vector :: [(HexString, HexString, HexString)]
test_vector = [
    ("0xa8510b592963f52d2d8b3413ab5822111c68d45370f6104a869fe6e72c58952fe6b420115a9dfadaae2956ad761db2f113", "0x5cc3704a552b40515909a220d3cb91e850d4d6c6c0180424e433d00400612ed6", "0x3477bfac3621680c30cf3ad59da9288bc53e46773af4911592c6f3b2ba9029861fac19b0fa0cc543731133346eb817cd7f3aa4cabac5e613919567bc9704af861c"),
    ("0x5680f1d195bbe2b0f1601a039ad23a6ad488c1f0949a68e84bffe398ad", "0xcaf03f0aa6862758e526dd76782a4afa7ad48533f00f8f9e8731d4a6b1656d9f", "0x659263cb4d54c653e5ef41550f959e15a71b01f19bce88750c8c3cad8ab293d12138db412f471df6c67575d49e9c25b6973a3405396a3e6985a3f391efee74751c"),
    ("0x487f9a", "0xa13bf6fdb2e2a5790f4a80d822180c7851b227d16c1893de2359dc98b1e27756", "0x143da940f9e144218979074cfde306d0697b3ddaec4c5e464a5aa093733f957776ba158ec4492a6cdc9951c6e0c1b10040916cb503ad240ed3b76677f9ac4ad41c"),
    ("0x958c2c5bafc5eb97004774f4c9b56aaecd37c06befac0042a93c67", "0x496b49cc02b63f927f2669a32dece10876e1a33dc0bd684f7643f849ecbe7c73", "0x34e768e959b89d305f16f796d0ff2da4e5923d6a6f33ba66b29471953e10be1a183743fb764f806ca8d97ad29dad94115d0368f374d5e085348267cc1c5bce411b"),
    ("0x02e2", "0x0425c9ac326acac4b93ff871f22c1dfe69c05038c2ac5b9eadb7e09743baa603", "0xbf52108b598a02aeeea1f9ee84947300985953edcfd07351f038bfe0de4c62df5cf467caaa7a33c3c4ebdf4aeaf05b1f4674f6b82b27e87fbf8d2a0980e68f1d1b"),
    ("0x74391586ff29f0638d7f7971640cefac3bfa3865ec5310e15a", "0x104432a31597e399f3f0f34e70b52a062feeb65aa3ad5383d2d7b90b0ab50126", "0x78c81fff1acdd1773badea5f35b0149e14b3dffe8de0e554b0edd80526acd55f1fc315b96afe5964df24f28b756f5651a6a52b2b9e0ffc87cda2a4a8f448a8951c"),
    ("0xd3ba712ef9dd8f8b37d805a9d1c2b5ee92db66dda0fd8582d55cec1b2519d18dfff2333e29081e80544af7244f2f22", "0x330ee59f080c59252218d1245029d8689b48b021dcf2c438938264bbc61d05af", "0xcf96a0f05e40d05ebcedbff0af522a68e6c846e63b44245a1208592ddf03cc12136ad522656f08fd838c55ff78e093d387d23b30b88a57e467636e63c786e04e1c"),
    ("0x1e99da3e7bb02e1f239bbf0b2ea113848673dd52da24c87f2f40", "0x52c58101fe861f9292fb6ad3df74551b5fb3feca9983b108645a5a9354bfbe99", "0x8eebcef599eb67e9379e7d6b074123628c1a65f1b754647704eb003fa61aff0a0e5c79fb022970fea80409e20a12bd4b8e1a79a787394a547fbc93c2d3c1e2dc1c"),
    ("0x1e973d1b5b158acc7d710099700c793e702b215a1fa534317d5c44406000fc727c", "0x1280413acfcd88a97b759e72e60069a1dc4f0a595e815aad9a1a83fa73f81af2", "0x7a77a37f2f4378dab5a0ba7f55b858a2a116f885f2eeab30dcd0b6d1f7286fbb7cbdccbd52721ce68fbcf2448a2f450a6bc6bc7f0027906259821bb3800133181b"),
    ("0x95da865540bd1336402a325a0435a920768f47d4b1ec0d89e0063f811872d1cb6423b5f4a4b931c9f41b216def", "0x0d537e54120ea923621225e3114b91a568a1abe7b7315b642017cad28cfad40b", "0xe04196529154ab89ceb598654f7d1ae178ecdcf73395aa8e8abb9200b504c39c58a93a6e502aaaddc2cbd712b436e9c9fb1010323927835ac54a7a77f11957f91c")
    ]

test_key :: HexString
test_key = "29461542faa1acbc968bcb332115e0537c023f8df416317602ca5d15ca12d02d"

spec :: Spec
spec = parallel $ do
    describe "Ethereum ECDSA" $ do
        it "can hash Ethereum prefixed message" $ for_ test_vector $ \(msg, msgHash, _) ->
            convert (hashMessage msg) `shouldBe` msgHash

        it "can sign Ethereum prefixed message" $ for_ test_vector $ \(msg, _, msgSig) ->
            signMessage (importKey test_key) msg `shouldBe` msgSig
