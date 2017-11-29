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
    (treeDepth':src:dest:[]) -> do
      let treeDepth = read treeDepth' :: Int

      srcExists <- doesDirectoryExist src
      dstExists <- doesDirectoryExist dest

      if treeDepth > 0 && treeDepth < 10 && srcExists && dstExists
      then do
        now <- getCurrentTime

        mv <- newMVar $ Progress now 0

        let progress x = modifyMVar_ mv (\(Progress s xs) -> pure $ Progress s (xs + x))

        putStrLn "Starting"
        putStrLn "_"

        forkIO $ forever $ do
          printProgress mv treeDepth
          threadDelay delay

        massmove progress treeDepth src dest

        putStrLn "_"
        putStrLn "All done."
        putStrLn ""
        printProgress mv treeDepth

      else
        printHelp

    _ -> printHelp
