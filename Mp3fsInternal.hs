module Mp3fsInternal
    (
     Mp3fsInternalData (rootdir, convertedFiles, tempdir)
    , ConvertedFile (..)
    , Mp3fsM
    , Mp3ConverterFunc
    , mp3TempDir
    , initInternalData
    , getConvertedHandle
    , mp3GetTempFile
    , mp3GetTempPipe
    , mp3GetConverter
    , mp3PossibleBaseNames
    , mp3FilesToConvert
    , mp3IsMusicFile
    , mp3FilterMusicFiles
    , runMp3fsM
    , runMp3fsM1
    , runMp3fsM2
    , runMp3fsM3
    , runMp3fsM4
    ) where

import Control.Concurrent.MVar
import Data.Map( fromList, member, empty, Map, (!), insert, keys)
import System.Unix.Directory (mkdtemp, removeRecursiveSafely)
import System.IO
import System.FilePath.Posix
import System.Posix.Files
import System.Directory
import Control.Monad
import Control.Monad.Reader


data Mp3fsInternalData = Mp3fsInternalData {
                                            rootdir :: FilePath,
                                            convertedFiles :: MVar (Map FilePath ConvertedFile ),
                                            converters :: Map String Mp3ConverterFunc,
                                            tempdir :: FilePath,
                                            tempfilecount :: MVar Int
                                            }


data ConvertedFile = ConvertedFile { -- The data type of a file which we have
                                     name :: FilePath,
                                     convertedPath :: FilePath,
                                     handle :: Maybe Handle,
                                     complete :: MVar Bool
                                   } | ConversionFailure | FileDoesNotExist

type Mp3fsM a = ReaderT Mp3fsInternalData IO a
type Mp3ConverterFunc = FilePath -> Mp3fsM ConvertedFile

runMp3fsM f r = runReaderT f r
runMp3fsM1 f r = \x -> runReaderT (f x) r
runMp3fsM2 f r = \x -> \y -> runReaderT (f x y) r
runMp3fsM3 f r = \x -> \y -> \z -> runReaderT (f x y z) r
runMp3fsM4 f r = \x -> \y -> \z -> \t -> runReaderT (f x y z t) r


mp3TempDir = ask >>= \x -> return (tempdir x)

possibleExtensions :: Mp3fsM [String]
possibleExtensions = (liftM (keys . converters)) ask

mp3FilterMusicFiles files =
    do
      converters <- (liftM converters) ask
      return (filter (\x -> (member (takeExtension x) converters)) files)

mp3PossibleBaseNames :: FilePath -> Mp3fsM [FilePath]
mp3PossibleBaseNames file =
    do
      extensions <- possibleExtensions
      return (map (replaceExtension file) extensions )

mp3FilesToConvert :: FilePath -> Mp3fsM (Maybe FilePath)
mp3FilesToConvert file = (mp3PossibleBaseNames file) >>= (filterM (\x -> liftIO (fileExist x)) ) >>= headOrNothing
    where
      headOrNothing [] = return Nothing
      headOrNothing (f:fs) = return (Just f)

mp3GetConverter :: String -> Mp3fsM Mp3ConverterFunc
mp3GetConverter ext =
    do
      convertersMap <- (liftM converters) ask
      return (convertersMap ! ext)

mp3IsMusicFile :: FilePath -> Mp3fsM Bool
mp3IsMusicFile file = (liftM converters) ask >>= \x -> return (member (takeExtension file) x)

instance Show ConvertedFile where
    show ConversionFailure = "ConversionFailure"
    show FileDoesNotExist = "FileDoesNotExist"
    show ConvertedFile { name = nm, handle = Nothing } = "Converted file: { name = " ++ nm ++ ", no handle}"
    show ConvertedFile { name = nm, handle = Just h } = "Converted file: { name = " ++ nm ++ ", has handle}"


getConvertedHandle :: ConvertedFile -> Mp3fsM Handle
getConvertedHandle ConvertedFile { handle = Just h, complete = c} = (liftIO (readMVar c)) >> return h
getConvertedHandle ConvertedFile { handle = Nothing, convertedPath = cp, complete = c } = (liftIO (readMVar c)) >> liftIO (openFile cp ReadMode)


initInternalData :: FilePath -> (Map String Mp3ConverterFunc) -> IO Mp3fsInternalData
initInternalData root converters = do
  convfiles <- newMVar (fromList [])
  mainTempDir <- getTemporaryDirectory
  dir <- mkdtemp (combine mainTempDir "mp3fsXXXXXX")
  count <- newMVar 0
  return Mp3fsInternalData { convertedFiles = convfiles,
                             tempdir = dir,
                             converters = converters,
                             tempfilecount = count,
                             rootdir = root
                           }

getNextTempCount :: Mp3fsM Int
getNextTempCount =
    do
      countMV <- (liftM tempfilecount) ask
      result <- liftIO (modifyMVar countMV (\x -> return (x, x+1) ))
      return result

mp3GetTempPipe :: Mp3fsM String
mp3GetTempPipe =
    do
      count <- getNextTempCount
      td <- mp3TempDir
      pipename <- return (combine td (show count))
      liftIO (createNamedPipe pipename (unionFileModes ownerReadMode ownerWriteMode))
      return pipename

mp3GetTempFile :: FilePath -> Mp3fsM (FilePath, Handle)
mp3GetTempFile filepath =
    do
      td <- mp3TempDir
      (finalPath, finalHandle) <- liftIO (openTempFile td (takeFileName filepath))
      return (finalPath, finalHandle)


