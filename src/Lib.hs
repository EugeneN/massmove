{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent.MVar (MVar, readMVar)
import           Control.Monad           (when)
import qualified Data.Hashable           as DH
import qualified Data.List.Split         as DLS
import           Data.Monoid             ((<>))
import           Data.Time.Clock         (UTCTime (..), diffUTCTime,
                                          getCurrentTime)
import qualified Numeric                 as Num
import           System.Directory        (copyFile, createDirectoryIfMissing,
                                          doesFileExist, getCurrentDirectory,
                                          listDirectory, makeAbsolute,
                                          removeFile)
import           System.FilePath         (FilePath, (</>))
import           System.FilePath.Posix   (takeFileName)


data Progress = Progress
  { starttime      :: UTCTime
  , itemsProcessed :: [(FilePath, FilePath)]
  }

imageFilenameSuffix :: FilePath -> FilePath
imageFilenameSuffix filename =
  let fhash    = Num.showHex (DH.hash filename) ""
      (x:xs)   = take 4 $ DLS.chunksOf 2 fhash
  in foldl (</>) x xs

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: <me> maxItems::Int src::FilePath dest::FilePath"
  putStrLn ""

printProgress :: MVar Progress -> Int -> IO ()
printProgress mv maxItems = do
  (Progress start xs) <- readMVar mv

  now <- getCurrentTime
  let deltat = diffUTCTime now start
      itemsDone = length xs
  putStr $ show deltat
  putStr " "
  putStr (show itemsDone)
  putStr "/"
  putStr (show maxItems)
  putStr " "
  -- putStr . show $ (itemsDone / 1.0) / (round deltat)
  putStrLn ""


massmove :: ((FilePath, FilePath) -> IO ()) -> Int -> FilePath -> FilePath -> IO ()
massmove progress count src dst = do
  cwd <- getCurrentDirectory
  absSrc <- makeAbsolute $ cwd </> src
  ds <- listDirectory absSrc

  putStrLn $ "cwd: " <> cwd
  putStrLn $ "absSrc: " <> absSrc

  move count cwd absSrc ds

  where
    move c cwd absSrc [] = print "src directory empty"
    move c cwd absSrc (srcfn:rest) = do
      let absSrcFn  = absSrc </> srcfn
          dstSuffix = imageFilenameSuffix srcfn
          absDstDir = cwd </> dst </> dstSuffix
          absDstFn  = cwd </> dst </> dstSuffix </> takeFileName srcfn

      putStrLn $ "about to move " <> absSrcFn <> " -> " <> absDstFn
      ex <- doesFileExist absSrcFn
      putStrLn $ absSrcFn <> " exists: " <> show ex

      when ex $ do
        createDirectoryIfMissing True absDstDir
        copyFile absSrcFn absDstFn
        removeFile absSrcFn
        progress (srcfn, absDstFn)

      when (c > 0) $ move (c - 1) cwd absSrc rest




