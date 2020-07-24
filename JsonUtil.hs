module JsonUtil where

import Control.Lens
import Data.Attoparsec.ByteString
import Data.Binary.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Waargonaut
import qualified Waargonaut.Decode as D
import Waargonaut.Encode.Builder
import Waargonaut.Encode.Builder.Whitespace
import Waargonaut.Types

jint :: Traversal Json Json Int Int
jint = _JNum . _1 . _JNumberInt

jtext :: Traversal Json Json Text Text
jtext = _JStr . _1 . _JStringText

jstring :: Traversal Json Json String String
jstring = jtext . iso T.unpack T.pack

jbool :: Traversal Json Json Bool Bool
jbool = _JBool . _1

jarr :: Traversal Json Json (JArray WS Json) (JArray WS Json)
jarr = _JArr . _1

readJsonFile :: String -> IO Json
readJsonFile file = do result <- D.parseWith parseOnly parseWaargonaut <$> B.readFile file
                       case result of
                         Left err -> error (show err)
                         Right json -> pure json

writeJsonFile :: String -> Json -> IO ()
writeJsonFile file = B.writeFile file . BL.toStrict . toLazyByteString . waargonautBuilder wsBuilder bsBuilder
