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

-----------
-- Types --
-----------

data Game
  = Game
    { gTime :: C.UTCTime
    , gLoser :: TL.Text
    , gWinner :: TL.Text
    , gBoardstate :: TL.Text
    }

type Games = M.Map Integer Game

data HistoryState
  = HistoryState
    { msId :: Integer
    , msGames :: Games
    }

------------------------
-- Runner and Routing --
------------------------

serverApp :: STM.TVar HistoryState -> S.ScottyM ()
serverApp mystateVar = do
  -- Our main page, which will display all of the tictactoe gmaes
  S.get "/" $ do
    games <- liftIO $ msGames <$> STM.readTVarIO mystateVar
    S.html $
      H.renderText $
        template
          "TicTacToe board - games"
          (gamesHtml games)

  -- A page for a specific game
  S.get "/game/:id" $ do
    pid <- S.param "id"
    games <- liftIO $ msGames <$> STM.readTVarIO mystateVar
    case M.lookup pid games of
      Just game ->
        S.html $
          H.renderText $
            template
              ("TicTacToe board - game " <> TL.pack (show pid))
              (gameHtml pid game)

      Nothing -> do
        S.status HTTP.notFound404
        S.html $
          H.renderText $
            template
              ("TicTacToe board - game " <> TL.pack (show pid) <> " not found.")
              "404 Game not found."

  -- A request to submit a new page
  S.post "/new" $ do
    winner <- S.param "winner"
    loser <- S.param "loser"
    boardState <- S.param "boardstate"
    time <- liftIO C.getCurrentTime
    pid <- liftIO $ newGame
      ( Game
        { gTime = time
        , gLoser = loser
        , gWinner = winner
        , gBoardstate = boardState
        }
      )
      mystateVar
    S.redirect ("/game/" <> TL.pack (show pid))

  -- A request to delete a specific game
  S.post "/game/:id/delete" $ do
    pid <- S.param "id"
    exists <- liftIO $ STM.atomically $ do
      mystate <- STM.readTVar mystateVar
      case M.lookup pid (msGames mystate) of
        Just{} -> do
          STM.writeTVar
            mystateVar
            ( mystate
              { msGames = M.delete pid (msGames mystate)
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

  -- css styling
  S.get "/style.css" $ do
    S.setHeader "Content-Type" "text/css; charset=utf-8"
    S.raw ".main { width: 900px; margin: auto; }"

newGame :: Game -> STM.TVar HistoryState -> IO Integer
newGame game mystateVar = do
  STM.atomically $ do
    mystate <- STM.readTVar mystateVar
    STM.writeTVar
      mystateVar
      ( mystate
        { msId = msId mystate + 1
        , msGames = M.insert (msId mystate) game (msGames mystate)
        }
      )
    pure (msId mystate)

-----------
-- Utils --
-----------

makeDummyGames :: IO Games
makeDummyGames = do
  time <- C.getCurrentTime
  pure $
    M.singleton
      0
      ( Game
        { gTime = time
        , gWinner = "Dummy winner"
        , gLoser = "Dummy loser"
        , gBoardstate = "bla bla bla..."
        }
      )

ppGame :: Game -> TL.Text
ppGame game =
  let
    header =
      TL.unwords
        [ "[" <> TL.pack (show (gTime game)) <> "]"
        , gWinner game
        , "by"
        , gLoser game
        ]
    seperator =
      TL.replicate (TL.length header) "-"
  in
    TL.unlines
      [ seperator
      , header
      , seperator
      , gBoardstate game
      , seperator
      ]

----------
-- HTML --
----------

type Html = H.Html ()

template :: TL.Text -> Html -> Html
template winner boardState =
  H.doctypehtml_ $ do
    H.head_ $ do
      H.meta_ [ H.charset_ "utf-8" ]
      H.title_ (H.toHtml winner)
      H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css"  ]
    H.body_ $ do
      H.div_ [ H.class_ "main" ] $ do
        H.h1_ [ H.class_ "logo" ] $
          H.a_ [H.href_ "/"] "TicTacToe Board"
        boardState

gamesHtml :: Games -> Html
gamesHtml games = do
  mapM_ (uncurry gameHtml) $ reverse $ M.toList games

gameHtml :: Integer -> Game -> Html
gameHtml pid game = do
  let title = TL.concat [gWinner game, " (W) vs. ", gLoser game, " (L)"]
  H.div_ [ H.class_ "game" ] $ do
    H.div_ [ H.class_ "game-header" ] $ do
      H.h3_ [ H.class_ "game-winner" ] $
        H.a_
          [H.href_ (TL.toStrict $ "/game/" <> TL.pack (show pid))]
          (H.toHtml title)

      H.span_ $ do
        H.p_ [ H.class_ "game-time" ] $ H.toHtml (TL.pack (show (gTime game)))

    H.div_ [H.class_ "game-boardstate"] $ do
      H.toHtml (gBoardstate game)

    H.form_
      [ H.method_ "post"
      , H.action_ (TL.toStrict $ "/game/" <> TL.pack (show pid) <> "/delete")
      , H.onsubmit_ "return confirm('Are you sure?')"
      , H.class_ "delete-game"
      ]
      ( do
        H.input_ [H.type_ "submit", H.value_ "Delete", H.class_ "deletebtn"]
      )
