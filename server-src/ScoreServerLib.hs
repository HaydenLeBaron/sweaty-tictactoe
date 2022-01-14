{-# language OverloadedStrings #-}

module ScoreServerLib where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Network.HTTP.Types as HTTP
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Lucid as H
import ScoreServerHTML as VIEW
import ScoreServerTypes

----------------------
-- RUNNER / ROUTER  --
----------------------

serverApp :: STM.TVar HistoryState -> S.ScottyM ()
serverApp mystateVar = do
  -- HOME: Displays the entire history of tic-tac-toe games
  S.get "/" $ do
    games <- liftIO $ hsGames <$> STM.readTVarIO mystateVar
    S.html $
      H.renderText $
        VIEW.template
          "TicTacToe board - games"
          (VIEW.gamesHtml games)

  -- Page for a specific tic-tac-toe game
  S.get "/game/:id" $ do
    gid <- S.param "id"
    games <- liftIO $ hsGames <$> STM.readTVarIO mystateVar
    case M.lookup gid games of
      Just game ->
        S.html $
          H.renderText $
            VIEW.template
              ("TicTacToe board - game " <> TL.pack (show gid))
              (VIEW.gameHtml gid game)

      Nothing -> do
        S.status HTTP.notFound404
        S.html $
          H.renderText $
            VIEW.template
              ("TicTacToe board - game " <> TL.pack (show gid) <> " not found.")
              "404 Game not found."

  -- A request to submit a new page
  S.post "/new" $ do
    winner <- S.param "winner"
    loser <- S.param "loser"
    boardState <- S.param "boardstate"
    time <- liftIO C.getCurrentTime
    gid <- liftIO $ newGame
      ( Game
        { gTime = time
        , gLoser = loser
        , gWinner = winner
        , gBoardstate = boardState
        }
      )
      mystateVar
    S.redirect ("/game/" <> TL.pack (show gid))

  -- A request to delete a specific game
  S.post "/game/:id/delete" $ do
    gid <- S.param "id"
    exists <- liftIO $ STM.atomically $ do
      mystate <- STM.readTVar mystateVar
      case M.lookup gid (hsGames mystate) of
        Just{} -> do
          STM.writeTVar
            mystateVar
            ( mystate
              { hsGames = M.delete gid (hsGames mystate)
              }
            )
          pure True

        Nothing ->
          pure False
    if exists
      then
        S.redirect "/"

      else do
        S.status HTTP.notFound404
        S.text "404 Not Found."

  -- Small amount of CSS styling
  S.get "/style.css" $ do
    S.setHeader "Content-Type" "text/css; charset=utf-8"
    S.raw ".main { width: 900px; margin: auto; }"

-----------
-- UTILS --
-----------

newGame :: Game -> STM.TVar HistoryState -> IO Integer
newGame game mystateVar = do
  STM.atomically $ do
    mystate <- STM.readTVar mystateVar
    STM.writeTVar
      mystateVar
      ( mystate
        { hsId = hsId mystate + 1
        , hsGames = M.insert (hsId mystate) game (hsGames mystate)
        }
      )
    pure (hsId mystate)

makeDummyGames :: IO Games
makeDummyGames = do
  time <- C.getCurrentTime
  pure $
    M.singleton
      0
      ( Game
        { gTime = time
        , gWinner = "DUMMY WINNER"
        , gLoser = "DUMMY LOSER"
        , gBoardstate = "[[]]"
        }
      )
