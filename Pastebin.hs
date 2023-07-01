{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Pastebin where

import Calamity.Types.LogEff
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import DiPolysemy hiding (error)
import Network.HTTP.Client
import Network.HTTP.Types
import Polysemy
import Polysemy.Reader
import TextShow
import Web.FormUrlEncoded

type PasteC r = Members [Embed IO, Reader Manager, LogEff] r

pastebin :: PasteC r => ByteString -> Sem r Text
pastebin bs = push "paste" do
  mgr <- ask
  let
    req = "POST https://paste.tomsmeding.com/paste"
     { requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
     , requestBody = RequestBodyLBS $ urlEncodeForm $ toForm
       [ ("name1" :: String, "")
       , ("code1", T.decodeUtf8With T.lenientDecode bs)
       , ("expire", "day")
       ]
     , redirectCount = 0
     }
  debug $ "Pasting " <> showt (BS.length bs) <> " bytes"
  resp <- liftIO $ httpNoBody req mgr
  debug $ "Got: " <> showt (FromStringShow resp)
  if responseStatus resp /= seeOther303
  then error $ "Unexpected response status: " ++ show (responseStatus resp)
  else case lookup "Location" $ responseHeaders resp of
    Nothing -> error "No location header"
    Just loc -> pure $ "https://paste.tomsmeding.com"
      <> T.decodeUtf8With T.lenientDecode loc
