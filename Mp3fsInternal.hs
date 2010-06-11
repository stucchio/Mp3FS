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
    , incReaders
    , decReaders
    , withConvertedFile
    , modifyConvertedFile
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
import Data.Time
import System.FilePath.Posix
import System.Posix.Files
import System.Directory
import Control.Monad
import Control.Monad.Reader


data Mp3fsInternalData = Mp3fsInternalData {
                                            rootdir :: FilePath,
                                            convertedFiles :: MVar (Map FilePath (MVar ConvertedFile)),
                                            converters :: Map String Mp3ConverterFunc,
                                            tempdir :: FilePath,
                                            tempfilecount :: MVar Int
                                            }


data ConvertedFile = ConvertedFile { -- The data type of a file which we have
                                     name :: FilePath,
                                     convertedPath :: FilePath,
                                     handle :: Maybe Handle,
                                     complete :: Bool,
                                     numReaders :: Int,
                                     lastAccess :: UTCTime
                                   } | ConversionFailure | FileDoesNotExist

instance Show ConvertedFile where
    show ConversionFailure = "ConversionFailure"
    show FileDoesNotExist = "FileDoesNotExist"
    show ConvertedFile { name = nm } = "Converted file: { name = " ++ nm ++ "}"

newConvertedFile name path handle complete = do
  time <- liftIO getCurrentTime
  return ConvertedFile {
                        name = name,
                        convertedPath = path,
                        handle = handle,
                        complete = complete,
                        numReaders = 0,
                        lastAccess = time
                       }

incReaders :: ConvertedFile -> Mp3fsM ConvertedFile
incReaders cf = return cf { numReaders = (numReaders cf) + 1 }

decReaders :: ConvertedFile -> Mp3fsM ConvertedFile
decReaders cf = do
  if (newCount == 0)
    then (do
            liftIO $ closeIfExists (handle cf)
            return cf { numReaders = newCount, handle = Nothing }
         )
    else return cf { numReaders = newCount }
    where
      oldCount = numReaders cf
      newCount = oldCount - 1
      closeIfExists Nothing = return ()
      closeIfExists (Just h) = hClose h

type Mp3fsM a = ReaderT Mp3fsInternalData IO a
type Mp3ConverterFunc = FilePath -> Mp3fsM (MVar ConvertedFile)

runMp3fsM  f r = runReaderT f r
runMp3fsM1 f r = \x -> runReaderT (f x) r
runMp3fsM2 f r = \x -> \y -> runReaderT (f x y) r
runMp3fsM3 f r = \x -> \y -> \z -> runReaderT (f x y z) r
runMp3fsM4 f r = \x -> \y -> \z -> \t -> runReaderT (f x y z t) r

mp3TempDir = ask >>= \x -> return (tempdir x)

possibleExtensions :: Mp3fsM [String]
possibleExtensions = (liftM (keys . converters)) ask

mp3FilterMusicFiles files = do
  converters <- (liftM converters) ask
  return (filter (\x -> (member (takeExtension x) converters)) files)

mp3PossibleBaseNames :: FilePath -> Mp3fsM [FilePath]
mp3PossibleBaseNames file = do
  extensions <- possibleExtensions
  return (map (replaceExtension file) extensions )

mp3FilesToConvert :: FilePath -> Mp3fsM (Maybe FilePath)
mp3FilesToConvert file = (mp3PossibleBaseNames file) >>= (filterM (\x -> liftIO (fileExist x)) ) >>= headOrNothing
    where
      headOrNothing [] = return Nothing
      headOrNothing (f:fs) = return (Just f)

mp3GetConverter :: String -> Mp3fsM Mp3ConverterFunc
mp3GetConverter ext = (liftM converters) ask >>= \cmap -> return (cmap ! ext)

mp3IsMusicFile :: FilePath -> Mp3fsM Bool
mp3IsMusicFile file = (liftM converters) ask >>= \x -> return (member (takeExtension file) x)


getConvertedHandle :: ConvertedFile -> Mp3fsM Handle
getConvertedHandle ConvertedFile { handle = hndl, complete = c, convertedPath = path} = do
  liftIO (do
            case hndl of
              Nothing -> openFile path ReadMode
              Just hd -> return hd
         )
      where

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
getNextTempCount = do
  countMV <- (liftM tempfilecount) ask
  result <- liftIO (modifyMVar countMV (\x -> return (x+1, x) ))
  return result

mp3GetTempPipe :: Mp3fsM String
mp3GetTempPipe = do
  count <- getNextTempCount
  td <- mp3TempDir
  pipename <- return (combine td ("pipe_" ++ (show count)))
  liftIO (createNamedPipe pipename (unionFileModes ownerReadMode ownerWriteMode))
  return pipename

mp3GetTempFile :: FilePath -> Mp3fsM (FilePath, Handle)
mp3GetTempFile filepath = do
  td <- mp3TempDir
  (finalPath, finalHandle) <- liftIO (openTempFile td (takeFileName filepath))
  return (finalPath, finalHandle)

makeAbsPathRelativeToRoot path = mp3RootDir >>= \root -> return (combine root (makeRelative "/" path) )


withConvertedFile :: FilePath -> (ConvertedFile -> Mp3fsM a) -> Mp3fsM a
withConvertedFile path func = do
  cfmvar <- getConvertedFile path
  cf <- liftIO $ takeMVar cfmvar
  result <- func cf
  liftIO $ putMVar cfmvar cf
  return result

modifyConvertedFile :: FilePath -> (ConvertedFile -> Mp3fsM (a, ConvertedFile)) -> Mp3fsM a
modifyConvertedFile path func = do
  cfmvar <- getConvertedFile path
  cf <- liftIO $ takeMVar cfmvar
  (result, newCf) <- func cf
  liftIO $ putMVar cfmvar newCf
  return result

getConvertedFile :: FilePath -> Mp3fsM (MVar ConvertedFile)
getConvertedFile filepath = do
  convertedFilesMVar <- (liftM convertedFiles) ask
  convertedfilemap <- liftIO (takeMVar convertedFilesMVar)
  if (member filepath convertedfilemap)
    then do
        liftIO (putMVar convertedFilesMVar convertedfilemap)
        return (convertedfilemap ! filepath)
    else do
        fileToConvert <- makeAbsPathRelativeToRoot filepath >>= mp3FilesToConvert
        case fileToConvert of
          Nothing -> (do
                        liftIO (putMVar convertedFilesMVar convertedfilemap)
                        result <- liftIO $ newMVar FileDoesNotExist
                        return result
                     )
          Just path -> (do
                          converter <- mp3GetConverter (takeExtension path)
                          cfmvar <- converter path
                          liftIO (putMVar convertedFilesMVar (insert filepath cfmvar convertedfilemap))
                          return cfmvar
                       )
