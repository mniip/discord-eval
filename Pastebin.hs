{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Pastebin where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Web.FormUrlEncoded

pastebin_addr = "tcp.st"
pastebin_port = 7777

pasteUrlLength :: Int
pasteUrlLength = 25

paste :: ByteString -> IO ByteString
paste text = do mgr <- newManager tlsManagerSettings
                let req = "POST https://paste.tomsmeding.com/paste"
                      { requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                      , requestBody = RequestBodyLBS $ urlEncodeForm $ toForm
                          [ ("name1" :: String, "")
                          , ("code1", T.decodeUtf8 text)
                          , ("expire", "day")
                          ]
                      , redirectCount = 0
                      }
                resp <- httpNoBody req mgr
                if responseStatus resp /= seeOther303
                then error $ "Unexpected response status: " ++ show (responseStatus resp)
                else case lookup "Location" $ responseHeaders resp of
                  Nothing -> error "No location header"
                  Just loc -> pure $ "https://paste.tomsmeding.com" <> loc
