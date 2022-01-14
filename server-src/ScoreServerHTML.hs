{-# language OverloadedStrings #-}

module ScoreServerHTML where

import qualified Lucid as H
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import ScoreServerTypes

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
