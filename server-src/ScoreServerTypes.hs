module ScoreServerTypes where

import qualified Data.Time.Clock as C
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M

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
    { hsId :: Integer
    , hsGames :: Games
    }

