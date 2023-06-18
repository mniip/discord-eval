{-# LANGUAGE RankNTypes #-}
module JsonUtil where

import Control.Lens
import Data.Aeson
import Data.Aeson.KeyMap
import Data.Attoparsec.ByteString
import Data.Binary.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Scientific
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Vector

jint :: Traversal Value Value Int Int
jint f (Number n)
  | Just i <- toBoundedInteger n = Number . fromIntegral <$> f i
jint _ x = pure x

jtext :: Traversal Value Value Text Text
jtext f (String s) = String <$> f s
jtext _ x = pure x

jstring :: Traversal Value Value String String
jstring = jtext . iso T.unpack T.pack

jbool :: Traversal Value Value Bool Bool
jbool f (Bool b) = Bool <$> f b
jbool _ x = pure x

jarr :: Traversal Value Value (Vector Value) (Vector Value)
jarr f (Array a) = Array <$> f a
jarr _ x = pure x

oat :: Key -> Traversal Value Value (Maybe Value) (Maybe Value)
oat k f (Object o) = Object <$> alterF f k o
oat _ _ x = pure x

readJsonFile :: String -> IO Value
readJsonFile file = do result <- eitherDecodeFileStrict' file
                       case result of
                         Left err -> error (show err)
                         Right json -> pure json

writeJsonFile :: String -> Value -> IO ()
writeJsonFile = encodeFile
