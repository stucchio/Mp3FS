module Mp3fsInternal
    (
     Mp3fsInternalData (..)
    , ConvertedFile (..)
    , initInternalData
    , getConvertedHandle
    ) where

import Control.Concurrent.MVar
import Data.Map( fromList, member, empty, Map, (!), insert, keys)
import System.Unix.Directory (mkdtemp, removeRecursiveSafely)
import System.IO
import System.FilePath.Posix
import System.Directory


data Mp3fsInternalData = Mp3fsInternalData {
                                            rootdir :: FilePath,
                                            convertedFiles :: MVar (Map FilePath ConvertedFile ),
                                            tempdir :: FilePath,
                                            tempfilecount :: MVar Int
                                            }

data ConvertedFile = ConvertedFile { -- The data type of a file which we have
                                     name :: FilePath,
                                     convertedPath :: FilePath,
                                     handle :: Maybe Handle,
                                     complete :: MVar Bool
                                   } | ConversionFailure | FileDoesNotExist

instance Show ConvertedFile where
    show ConversionFailure = "ConversionFailure"
    show FileDoesNotExist = "FileDoesNotExist"
    show ConvertedFile { name = nm, handle = Nothing } = "Converted file: { name = " ++ nm ++ ", no handle}"
    show ConvertedFile { name = nm, handle = Just h } = "Converted file: { name = " ++ nm ++ ", has handle}"


getConvertedHandle ConvertedFile { handle = Just h, complete = c} = (readMVar c) >> return h
getConvertedHandle ConvertedFile { handle = Nothing, convertedPath = cp, complete = c } = (readMVar c) >> openFile cp ReadMode


initInternalData :: FilePath -> IO Mp3fsInternalData
initInternalData root = do
  convfiles <- newMVar (fromList [])
  mainTempDir <- getTemporaryDirectory
  dir <- mkdtemp (combine mainTempDir "mp3fsXXXXXX")
  count <- newMVar 0
  return Mp3fsInternalData { convertedFiles = convfiles,
                             tempdir = dir,
                             tempfilecount = count,
                             rootdir = root
                           }

getNextTempCount :: Mp3fsInternalData -> IO Int
getNextTempCount internal = modifyMVar (tempfilecount internal) (\x -> return (x, x+1) )
