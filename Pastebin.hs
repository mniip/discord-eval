{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Pastebin where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString as NBS

pastebin_addr = "tcp.st"
pastebin_port = 7777

pasteUrlLength :: Int
pasteUrlLength = 25

paste :: ByteString -> IO ByteString
paste text = do addrs <- getAddrInfo Nothing (Just pastebin_addr) (Just $ show pastebin_port)
                case addrs of
                  [] -> error "Could not resolve hostname"
                  addr:_ -> do
                    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                    connect sock (addrAddress addr)
                    sendAll sock text
                    recvUrl sock ""
  where
    sendAll sock bs = do i <- NBS.send sock bs
                         if | i == BS.length bs -> pure ()
                            | i > 0 -> sendAll sock (BS.drop i bs)
                            | otherwise -> pure ()
    
    recvUrl sock buf | BS.isInfixOf "\n" buf
                     , (line, rest) <- BS.breakSubstring "\n" buf
                     , (word, param) <- BS.breakSubstring " " line
                     = if | word == "URL" -> pure $ BS.tail param
                          | word == "ERROR" -> error $ show param
                          | otherwise -> recvUrl sock (BS.tail rest)
                     | otherwise
                     = do bs <- NBS.recv sock 4096
                          if BS.null bs
                          then error "No URL received"
                          else recvUrl sock (BS.append buf bs)
