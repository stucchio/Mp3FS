module Mp3fsInternal
    (
     Mp3fsInternalData (rootdir, convertedFiles, tempdir)
    , ConvertedFile (..)
    , Mp3fsM
    , Mp3ConverterFunc
    , mp3TempDir
    , mp3RootDir
    , initInternalData
    , getConvertedHandle
    , mp3GetTempFile
    , mp3GetTempPipe
    , mp3GetConverter
    , mp3PossibleBaseNames
    , mp3FilesToConvert
    , mp3IsMusicFile
    , mp3FilterMusicFiles
    , makeAbsPathRelativeToRoot
    , newConvertedFile
    , getConvertedFile
    , incReaders
    , decReaders
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
                                     handle :: MVar (Maybe Handle),
                                     complete :: MVar Bool,
                                     numReaders :: MVar Int
                                   } | ConversionFailure | FileDoesNotExist

newConvertedFile name path handle complete =
    do
      mvb <- (case complete of
                Just c -> liftIO (newMVar c)
                Nothing -> liftIO newEmptyMVar)
      nr <- liftIO (newMVar 0)
      mvh <- liftIO (newMVar handle)
      return ConvertedFile {
                            name = name,
                            convertedPath = path,
                            handle = mvh,
                            complete = mvb,
                            numReaders = nr
                           }



incReaders :: ConvertedFile -> Mp3fsM Int
incReaders cf = liftIO (modifyMVar (numReaders cf) (\x -> return (x+1, x+1)))

decReaders :: ConvertedFile -> Mp3fsM Int
decReaders cf = liftIO (modifyMVar (numReaders cf) (\x -> return (x-1, x-1)))


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
    show ConvertedFile { name = nm } = "Converted file: { name = " ++ nm ++ "}"


getConvertedHandle :: ConvertedFile -> Mp3fsM Handle
getConvertedHandle ConvertedFile { handle = h, complete = c, convertedPath = path} =
    do
      liftIO (do
                hndl <-  (takeMVar h)
                case hndl of
                  Nothing -> do
                            handle <- openFile path ReadMode
                            putMVar h (Just handle)
                            return handle
                  Just hd -> do
                            putMVar h (Just hd)
                            return hd
             )

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

mp3RootDir :: Mp3fsM FilePath
mp3RootDir = (liftM rootdir) ask

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


makeAbsPathRelativeToRoot path =
    do
      root <- mp3RootDir
      return (combine root (makeRelative "/" path) )

getConvertedFile :: FilePath -> Mp3fsM ConvertedFile
getConvertedFile filepath =
    do
      convertedFilesMVar <- (liftM convertedFiles) ask
      convertedfilemap <- liftIO (takeMVar convertedFilesMVar)
      if (member filepath convertedfilemap)
          then do
                liftIO (putMVar convertedFilesMVar convertedfilemap)
                return (convertedfilemap ! filepath)
          else do
                absFilePath <- makeAbsPathRelativeToRoot filepath
                fileToConvert <- mp3FilesToConvert absFilePath
                case fileToConvert of
                  Nothing ->
                      do
                        liftIO (putMVar convertedFilesMVar convertedfilemap)
                        return FileDoesNotExist
                  Just path ->
                       do
                         converter <- mp3GetConverter (takeExtension path)
                         convertedFile <- converter path
                         liftIO (putMVar convertedFilesMVar (insert filepath convertedFile convertedfilemap))
                         return convertedFile
