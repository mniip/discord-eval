module Format where

import Control.Arrow
import Data.ByteString (ByteString)
import Data.Char
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T hiding (replace)
import Data.Ord
import Polysemy

import Pastebin

formatCodeBlock :: ByteString -> Text
formatCodeBlock bs
  | txt <- T.decodeUtf8With T.lenientDecode bs
  , txt' <- T.replace "``" "``\x200D" $ T.filter goodChar txt
  = T.concat
    [ "```ansi\n"
    , txt'
    , if "`" `T.isSuffixOf` txt' then "\x200D" else ""
    , if T.null txt' then "\n" else ""
    , "```"
    ]
  where
    goodChar = \case
      '\n' -> True
      '\x1B' -> True
      c -> not $ isControl c

formatPaste :: Text -> Text
formatPaste link = T.concat ["<", link, ">\n"]

formatResults :: PasteC r => Int -> [ByteString] -> Sem r Text
formatResults maxChars results = go [] $
  sortOn (Down . T.length . snd . snd) $
    zip [0 :: Int ..] $ (id &&& formatCodeBlock) <$> results
  where
    totalSize pasted blocks =
      sum (T.length . snd <$> pasted)
      + sum (T.length . snd . snd <$> blocks)

    go pasted blocks
      | totalSize pasted blocks > maxChars
      , (i, (result, _)):bs <- blocks
      = do
        link <- pastebin result
        go ((i, formatPaste link):pasted) bs
      | otherwise
      = pure $ T.concat $ snd <$> (sortOn fst pasted ++ (second snd <$> blocks))
