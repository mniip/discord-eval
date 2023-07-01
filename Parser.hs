{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Parser where

import Config
import Control.Monad
import Data.Char
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Polysemy
import Polysemy.Reader
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

codeBlock :: Parsec Void Text (Maybe Text, Text)
codeBlock = do
  void $ string "```"
  choice
    [ try do
      language <- takeWhile1P Nothing \case
        '+' -> True
        '-' -> True
        '.' -> True
        c -> isAscii c && isAlphaNum c
      void $ takeWhile1P Nothing (== '\n')
      (Just language,) <$> finish
    , do
      void $ takeWhileP Nothing (== '\n')
      (Nothing,) <$> finish
    ]
  where
    finish = T.concat <$> someTill
      (takeWhile1P Nothing (`notElem` ['\n', '`'])
        <|> string "\n" <|> string "`")
      (try $ takeWhileP Nothing (== '\n') *> string "```")

inlineCode :: Parsec Void Text Text
inlineCode = choice
  [ try do
    void $ string "``"
    (content, end) <- manyTill_
      (takeWhile1P Nothing (/= '`') <|> string "`")
      (try $ takeWhile1P Nothing (/= '`')
        <* string "``" <* notFollowedBy (string "`"))
    pure $ stripLeft $ stripRight $ T.concat content <> end
  , do
    void $ string "`"
    (content, end) <- manyTill_
      (takeWhile1P Nothing (/= '`') <|> string "`")
      (try $ takeWhile1P Nothing (/= '`')
        <* string "`" <* notFollowedBy (string "`"))
    pure $ stripLeft $ stripRight $ T.concat content <> end
  ]
  where
    stripLeft (T.stripPrefix " " -> Just t) | "`" `T.isPrefixOf` t = t
    stripLeft t = t
    stripRight (T.stripSuffix " " -> Just t) | "`" `T.isSuffixOf` t = t
    stripRight t = t

parseMessage
  :: Member (Reader EvalConfig) r => Text -> Sem r [Command]
parseMessage input = do
  config <- ask
  let
    Blocks maxBlocks = config.blockCountLimit

    parser = take maxBlocks . concat <$> many do
      choice
        [ try do
          (mLang, content) <- codeBlock
          let
            wrapper = fromMaybe config.defaultCodeBlock
              (mLang >>= (`M.lookup` config.codeBlockLanguages))
          pure [EvalBlock wrapper.interpreter $ wrap wrapper content]
        , try do
          content <- inlineCode
          let wrapper = config.defaultInline
          pure [EvalLine wrapper.interpreter $ wrap wrapper content]
        , try do
          void $ string "!reset"
          mLang <- optional $ try $ space1 *> takeWhileP Nothing isAlpha
          pure [Reset $
            maybe config.defaultInline.interpreter InterpreterName mLang]
        , [] <$ anySingle <* takeWhileP Nothing (`notElem` ['!', '`'])
        ]

    wrap :: Wrapper -> Text -> Text
    wrap wrapper content = addPrefix $ addSuffix $ stripControl content
      where
        addPrefix = case wrapper.prefix of
          Just p -> (p <>)
          _ -> id
        addSuffix = case wrapper.suffix of
          Just s -> (<> s)
          _ -> id
        stripControl = case wrapper.stripControl of
          Just True -> T.map \case
            c | isControl c -> ' '
              | otherwise -> c
          _ -> id

  pure $ case parse (parser <* eof) "" input of
    Left errs -> error $ errorBundlePretty errs
    Right blocks -> blocks
