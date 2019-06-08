module Telegram.Bot.API.InlineMode where

import           Control.Monad.Reader

import           Telegram.Bot.API        as Telegram
import           Telegram.Bot.Simple.Eff

currentInlineQuery :: BotM (Maybe InlineQuery)
currentInlineQuery = do
  mupdate <- asks botContextUpdate
  pure $ updateInlineQuery =<< mupdate

answerInlineQuery :: [InlineQueryResult] -> BotM ()
answerInlineQuery results = do
  mqueryToAnswer <- currentInlineQuery
  case mqueryToAnswer of
      Just queryToAnswer -> do
        let req = AnswerInlineQueryRequest (inlineQueryId queryToAnswer) results
        void $ liftClientM $ Telegram.answerInlineQuery req
      Nothing -> liftIO $ putStrLn "No inline query to answer"
