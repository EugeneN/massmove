module Main where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (modifyMVar_, newMVar)
import           Control.Monad           (forever)
import           Data.Monoid             ((<>))
import           Data.Time.Clock         (getCurrentTime)
import           Lib
import           System.Directory        (doesDirectoryExist)
import           System.Environment      (getArgs)

delay = 1000000



main :: IO ()
main = do
  as <- getArgs

  case as of
    (maxItems':src:dest:rest) -> do
      let maxItems = read maxItems' :: Int

      srcExists <- doesDirectoryExist src
      dstExists <- doesDirectoryExist dest

      if maxItems > 0 && maxItems < 1000000 && srcExists && dstExists
      then do
        now <- getCurrentTime

        mv <- newMVar $ Progress now 0

        let progress sd = modifyMVar_ mv (\(Progress s xs) -> pure $ Progress s (xs + sd))

        putStrLn "Starting"
        putStrLn "_"

        forkIO $ forever $ do
          printProgress mv maxItems
          threadDelay delay

        massmove progress maxItems src dest

        putStrLn "_"
        putStrLn "All done."
        putStrLn ""
        printProgress mv maxItems

      else
        printHelp

    _ -> printHelp



