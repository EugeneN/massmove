{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent.MVar   (MVar, readMVar)
import           Control.Monad             (unless, when)
import qualified Crypto.Hash               as CH
import           Data.DirStream
import qualified Data.List.Split           as DLS
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Data.Time.Clock           (UTCTime (..), diffUTCTime,
                                            getCurrentTime)
import qualified Filesystem.Path           as FP
import           Filesystem.Path.CurrentOS (fromText, toText)
import           Pipes
import           Pipes.Safe
import           Prelude                   hiding (FilePath, concat)
import           System.Directory          (copyFile, createDirectoryIfMissing,
                                            doesFileExist, getCurrentDirectory,
                                            listDirectory, makeAbsolute,
                                            removeFile)
import qualified System.FilePath           as SFP
import           System.FilePath.Posix     (takeFileName)


data Progress = Progress
  { starttime      :: UTCTime
  , itemsProcessed :: Int
  }

imageFilenameSuffix :: SFP.FilePath -> SFP.FilePath
imageFilenameSuffix filename =
  let hashit :: SFP.FilePath -> String
      hashit y = show (CH.hash (encodeUtf8 . T.pack $ y) :: CH.Digest CH.SHA256)

      hexfhash = hashit filename
      (x:xs) = take 4 $ DLS.chunksOf 2 hexfhash

  in foldl (SFP.</>) x xs -- (fmap sfp2fp xs)

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: <me> maxItems::Int src::FilePath dest::FilePath"
  putStrLn ""

printProgress :: MVar Progress -> Int -> IO ()
printProgress mv maxItems = do
  (Progress start xs) <- readMVar mv

  now <- getCurrentTime
  let deltat = diffUTCTime now start
      itemsDone = xs
  putStr $ show deltat
  putStr " "
  putStr (show itemsDone)
  putStr " / "
  putStr . show $ (itemsDone `div` (let x = round deltat in if x == 0 then 1 else x))
  putStrLn " files/sec"

sfp2fp = fromText . T.pack
fp2sft x = case toText x of
  Right s -> T.unpack s
  Left s -> T.unpack s

massmove :: (Int -> IO ()) -> Int -> SFP.FilePath -> SFP.FilePath -> IO ()
massmove progress count src dst = do
  cwd <- getCurrentDirectory
  absSrc <- makeAbsolute $ cwd SFP.</> src

  let absSrc' = sfp2fp absSrc

  runSafeT $ runEffect $
    for (every (childOf absSrc')) $ \z ->
      liftIO $ move count cwd dst absSrc z

  where
    move :: Int -> SFP.FilePath -> SFP.FilePath -> SFP.FilePath -> FP.FilePath -> IO ()
    move c cwd dst absSrc srcfn' = do
      let srcfn     = fp2sft srcfn'
          absSrcFn  = absSrc SFP.</> srcfn
          dstSuffix = imageFilenameSuffix srcfn
          absDstDir = cwd SFP.</> dst SFP.</> dstSuffix
          absDstFn  = absDstDir SFP.</> takeFileName srcfn

      ex <- doesFileExist absSrcFn

      when ex $ do
        createDirectoryIfMissing True absDstDir
        copyFile absSrcFn absDstFn
        removeFile absSrcFn
        progress 1





