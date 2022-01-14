import ScoreServerTypes as SST
import ScoreServerLib as SSL
import qualified Control.Concurrent.STM as STM
import qualified Web.Scotty as S


main :: IO ()
main = do
  games <- SSL.makeDummyGames
  gameHistoryState <- STM.newTVarIO SST.HistoryState{hsId = 1, hsGames = games}
  S.scotty 3000 (SSL.serverApp gameHistoryState)
