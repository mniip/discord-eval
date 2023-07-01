{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Frontend where

import Cache
import Cache qualified as ActiveMessage (ActiveMessage(..))
import Calamity hiding (Embed, Member)
import Calamity qualified as CreateMessageOptions (CreateMessageOptions(..))
import Calamity.Cache.Eff
import Calamity.Internal.Utils (MaybeNull(..))
import Calamity.Types.Model.Channel.UpdatedMessage
import Config
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Df1 hiding (Message)
import DiPolysemy
import Parser
import Polysemy
import Polysemy.AtomicState
import Polysemy.Reader
import Polysemy.Resource
import TextShow
import Types
import UpdateChan

cachedMessage
  :: Members [Embed IO, Reader EvalConfig, AtomicState EvalState, CacheEff] r
  => Message -> Sem r (Maybe ActiveMessage)
cachedMessage msg = do
  config :: EvalConfig <- ask
  case (msg.guildID, msg.author) of
    (Just gId, User' _)
      | config.isTest == gId `S.member` config.testGuilds -> do
          commands <- parseRequest msg.mentions msg.content
          let
            am = ActiveMessage
              { id = msg.id
              , updated = msg.timestamp
              , channel = msg.channelID
              , commands
              , response = Nothing
              }
          upsertMessage am
          pruneMessages
          pure $ Just am
    _ -> pure Nothing

parseRequest
  :: Members '[Reader EvalConfig, CacheEff] r
  => [User] -> Text -> Sem r (Maybe (NonEmpty Command))
parseRequest [] _ = pure Nothing
parseRequest mentions content = getBotUser >>= \case
  Just me | any (\m -> m.id == me.id) mentions
    -> NE.nonEmpty <$> parseMessage content
  _ -> pure Nothing

processChangedCommands
  :: (BotC r, Members [Reader EvalConfig, AtomicState EvalState] r)
  => UpdateChan (Snowflake Message) Request
  -> ActiveMessage
  -> Sem r ()
processChangedCommands chan am = do
  response <- liftIO newEmptyMVar
  working <- writeUpdateChan chan am.id Request
    { commands = am.commands
    , response
    }

  case am.commands of
    Nothing -> for_ am.response \respId -> do
      void $ invoke $ DeleteMessage am.channel respId
      upsertMessage am { ActiveMessage.response = Nothing }
      pruneMessages
      debug $ "Deleted previous response: " <> showt respId

    Just _ -> do
      config :: EvalConfig <- ask
      if working
      then do
        debug @String "starting imediately"
        void $ invoke $ TriggerTyping am.channel
      else do
        debug @String "queued"
        void $ invoke $ CreateReaction am.channel am.id config.reactWait

      mVar <- liftIO (takeMVar response)
      case mVar of
        Nothing -> debug @String "Overridden by another request"
        Just var -> do
          unless working $ void $ invoke $
            DeleteOwnReaction am.channel am.id config.reactWait
          content <- liftIO (takeMVar var)
          debug $ "Got response: " <> showt content

          lookupMessage am.id >>= traverse_ \am' -> case am'.response of
            Nothing -> do
              invoke $ CreateMessage am'.channel def
                  { CreateMessageOptions.content = Just
                    if T.null content then config.emptyOutput else content
                  }
              >>= \case
                Right respMsg -> do
                  upsertMessage am' { ActiveMessage.response = Just respMsg.id }
                  pruneMessages
                  debug $ "Responded with: " <> showt respMsg.id
                Left _ -> pure ()
            Just respId -> do
              void $ invoke $ EditMessage am'.channel respId $
                editMessageContent $ Just
                  if T.null content then config.emptyOutput else content
              debug $ "Updated previous response: " <> showt respId

onMessageCreate
  :: (BotC r, Members '[Reader EvalConfig, AtomicState EvalState] r)
  => UpdateChan (Snowflake Message) Request -> Message -> Sem r ()
onMessageCreate chan msg = cachedMessage msg >>= traverse_ \am -> do
  debug $ "#" <> showt am.channel <> " : "
    <> showt (NE.toList <$> am.commands)
  processChangedCommands chan am

onMessageUpdate
  :: (BotC r, Members '[Reader EvalConfig, AtomicState EvalState] r)
  => UpdateChan (Snowflake Message) Request
  -> UpdatedMessage
  -> UTCTime
  -> Sem r ()
onMessageUpdate chan msg timestamp =
  for_ msg.content \content -> for_ msg.mentions \mentions ->
    lookupMessage msg.id >>= traverse_ \am -> do
      commands <- parseRequest mentions content
      when (commands /= am.commands) do
        let
          am' = am
            { ActiveMessage.updated = timestamp
            , ActiveMessage.commands
            }
        upsertMessage am'
        pruneMessages
        processChangedCommands chan am'

onMessageDelete
  :: (BotC r, Members '[Reader EvalConfig, AtomicState EvalState] r)
  => UpdateChan (Snowflake Message) Request
  -> Snowflake Message
  -> Sem r ()
onMessageDelete chan msgId = lookupMessage msgId >>= traverse_ \am ->
  when (isJust am.commands) do
    now <- liftIO getCurrentTime
    let
      am' = am
        { updated = now
        , ActiveMessage.commands = Nothing
        }
    upsertMessage am'
    pruneMessages
    processChangedCommands chan am'

frontend
  :: (BotC r, Members '[Embed IO, Resource, Reader EvalConfig] r)
  => UpdateChan (Snowflake Message) Request -> Sem r ()
frontend chan = bracket_
  (info @String "Starting frontend thread")
  (info @String "Closing frontend thread")
  $ void $ atomicStateToIO @EvalState def $ push "frontend" do
    void $ react @'MessageCreateEvt \(msg, _, _) ->
      push (segment $ showt msg.id) $
        push (segment $ showt $ FromStringShow msg.timestamp) $
          push "create" $ onMessageCreate chan msg
    void $ react @'RawMessageUpdateEvt \(msg, _, _) ->
      for_ msg.editedTimestamp $ \case
        WasNull -> pure ()
        NotNull timestamp ->
          push (segment $ showt msg.id) $
            push (segment $ showt $ FromStringShow timestamp) $
              push "update" $ onMessageUpdate chan msg timestamp
    void $ react @'RawMessageDeleteEvt \msgId ->
      push (segment $ showt msgId) $
        push "delete" $ onMessageDelete chan msgId
