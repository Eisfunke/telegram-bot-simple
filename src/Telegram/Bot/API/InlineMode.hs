module Telegram.Bot.API.InlineMode where

import           Control.Monad.Reader

import           Telegram.Bot.API        as Telegram
import           Telegram.Bot.Simple.Eff

import Data.Aeson
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

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
        liftIO $ putStrLn $ unpack $ decodeUtf8 $ encode req
        res <- liftClientM $ Telegram.answerInlineQuery req
        liftIO $ putStrLn $ "Res:" ++ show res
        return ()
      Nothing -> liftIO $ putStrLn "No inline query to answer"
