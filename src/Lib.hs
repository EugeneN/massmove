{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent.MVar   (MVar, readMVar)
import           Control.Monad             (unless, when)
import           Crypto.Hash               (Digest, SHA256, hash)
import           Data.DirStream            (childOf)
import           Data.List.Split           (chunksOf)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Data.Time.Clock           (UTCTime (..), diffUTCTime,
                                            getCurrentTime)
import qualified Filesystem.Path           as FP
import           Filesystem.Path.CurrentOS (fromText, toText)
import           Pipes                     (every, for, liftIO, runEffect)
import           Pipes.Safe                (runSafeT)
import           Prelude
import           System.Directory          (copyFile, createDirectoryIfMissing,
                                            doesFileExist, getCurrentDirectory,
                                            listDirectory, makeAbsolute,
                                            removeFile)
import qualified System.FilePath           as SFP
import           System.FilePath.Posix     (takeFileName)


data Progress = Progress
  { startTime      :: UTCTime
  , itemsProcessed :: Int
  }

imageFilenameSuffix :: SFP.FilePath -> Int -> SFP.FilePath
imageFilenameSuffix filename treeDepth =
  let hashit :: SFP.FilePath -> String
      hashit y = show (hash (encodeUtf8 . T.pack $ y) :: Digest SHA256)

      hexfhash = hashit filename
      (x:xs) = take treeDepth $ chunksOf 2 hexfhash

  in foldl (SFP.</>) x xs

massmove :: (Int -> IO ()) -> Int -> SFP.FilePath -> SFP.FilePath -> IO ()
massmove progress treeDepth src dst = do
  cwd <- getCurrentDirectory
  absSrc <- makeAbsolute $ cwd SFP.</> src

  runSafeT $ runEffect $
    for (every (childOf $ sfp2fp absSrc)) $ \z ->
      liftIO $ move treeDepth cwd dst absSrc z

  where
    move :: Int -> SFP.FilePath -> SFP.FilePath -> SFP.FilePath -> FP.FilePath -> IO ()
    move treeDepth cwd dst absSrc srcfn' = do
      let srcfn     = fp2sft srcfn'
          absSrcFn  = absSrc SFP.</> srcfn
          dstSuffix = imageFilenameSuffix srcfn treeDepth
          absDstDir = cwd SFP.</> dst SFP.</> dstSuffix
          absDstFn  = absDstDir SFP.</> takeFileName srcfn

      ex <- doesFileExist absSrcFn

      when ex $ do
        createDirectoryIfMissing True absDstDir
        copyFile absSrcFn absDstFn
        removeFile absSrcFn
        progress 1

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: <me> treeDepth::Int src::FilePath dest::FilePath"
  putStrLn ""

printProgress :: MVar Progress -> Int -> IO ()
printProgress mv treeDepth = do
  (Progress start itemsDone) <- readMVar mv

  now <- getCurrentTime
  let deltat = diffUTCTime now start
  putStrLn $ show deltat <> " " <> (show itemsDone) <> " / "
          <> (show $ itemsDone `div` (let x = round deltat in if x == 0 then 1 else x))
          <> " files/sec"

sfp2fp = fromText . T.pack
fp2sft x = case toText x of
  Right s -> T.unpack s
  Left s  -> T.unpack s
