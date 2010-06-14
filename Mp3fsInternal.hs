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
    , cleanupPeriodically
    , runMp3fsM
    , runMp3fsM1
    , runMp3fsM2
    , runMp3fsM3
    , runMp3fsM4
    ) where

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map --Data.Map( member, empty, Map, (!), insert, keys, findMin, null)
import Data.Map ( (!) )
import System.Unix.Directory (mkdtemp, removeRecursiveSafely)
import System.IO
import System.Time
import System.FilePath.Posix
import System.Posix.Files
import System.Directory
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E

data Mp3fsInternalData = Mp3fsInternalData {
                                            rootdir :: FilePath,
                                            convertedFiles :: MVar (Map.Map FilePath (MVar ConvertedFile)),
                                            converters :: Map.Map String Mp3ConverterFunc,
                                            tempdir :: FilePath,
                                            tempfilecount :: MVar Int,
                                            cleanupDelay :: Int, -- cleanup delay in seconds
                                            cleanupList :: MVar (Map.Map ClockTime FilePath )
                                            }


data ConvertedFile = ConvertedFile { -- The data type of a file which we have
                                     name :: FilePath,
                                     convertedPath :: FilePath,
                                     handle :: Maybe Handle,
                                     complete :: Bool,
                                     numReaders :: Int,
                                     lastAccess :: ClockTime
                                   } | ConversionFailure | FileDoesNotExist | ConversionDeleted

instance Show ConvertedFile where
    show ConversionFailure = "ConversionFailure"
    show FileDoesNotExist = "FileDoesNotExist"
    show ConvertedFile { name = nm } = "Converted file: { name = " ++ nm ++ "}"

newConvertedFile name path handle complete = do
  time <- liftIO getClockTime
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
possibleExtensions = (liftM (Map.keys . converters)) ask

mp3FilterMusicFiles files = do
  converters <- (liftM converters) ask
  return (filter (\x -> (Map.member (takeExtension x) converters)) files)

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
mp3IsMusicFile file = (liftM converters) ask >>= \x -> return (Map.member (takeExtension file) x)


getConvertedHandle :: ConvertedFile -> Mp3fsM Handle
getConvertedHandle ConvertedFile { handle = hndl, complete = c, convertedPath = path} = do
  liftIO (do
            case hndl of
              Nothing -> openFile path ReadMode
              Just hd -> return hd
         )
      where

initInternalData :: FilePath -> (Map.Map String Mp3ConverterFunc) -> IO Mp3fsInternalData
initInternalData root converters = do
  convfiles <- newMVar Map.empty
  mainTempDir <- getTemporaryDirectory
  dir <- mkdtemp (combine mainTempDir "mp3fsXXXXXX")
  count <- newMVar 0
  cl <- liftIO $ newMVar Map.empty
  return Mp3fsInternalData { convertedFiles = convfiles,
                             tempdir = dir,
                             converters = converters,
                             tempfilecount = count,
                             rootdir = root,
                             cleanupDelay = 5*60,
                             cleanupList = cl
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

-- Puts the file path into the cleanup request queue.
addCleanupRequest time path = do
  (cl, delay)  <- ask >>= \internal -> return ( (cleanupList internal), (cleanupDelay internal) )
  liftIO $ modifyMVar_ cl (\clist -> return (Map.insert (addSecondsToTime time (toInteger delay)) path clist))

addSecondsToTime (TOD sec pico) delay = TOD (sec + delay) pico
secDelayBetween (TOD sec1 _) (TOD sec2 _) = sec2 - sec1

mapFilterM_ :: Monad m => (Ord k) => ((k,v) -> m Bool) -> (Map.Map k v) -> m (Map.Map k v)
mapFilterM_ func mp = do
  asLst <- return (Map.toList mp)
  resultLst <- filterM func asLst
  return (Map.fromList resultLst)

withConvertedFile :: FilePath -> (ConvertedFile -> Mp3fsM a) -> Mp3fsM a
withConvertedFile path func = do
  cfmvar <- getConvertedFile path
  cf <- liftIO $ takeMVar cfmvar
  result <- func cf
  time <- liftIO $ getClockTime
  liftIO $ putMVar cfmvar (cf { lastAccess = time})
  return result

modifyConvertedFile :: FilePath -> (ConvertedFile -> Mp3fsM (a, ConvertedFile)) -> Mp3fsM a
modifyConvertedFile path func = do
  cfmvar <- getConvertedFile path
  cf <- liftIO $ takeMVar cfmvar
  (result, newCf) <- func cf
  time <- liftIO $ getClockTime
  liftIO $ putMVar cfmvar (newCf { lastAccess = time})
  return result

getConvertedFile :: FilePath -> Mp3fsM (MVar ConvertedFile)
getConvertedFile filepath = do
  convertedFilesMVar <- (liftM convertedFiles) ask
  convertedfilemap <- liftIO (takeMVar convertedFilesMVar)
  if (Map.member filepath convertedfilemap)
    then do
        liftIO (putMVar convertedFilesMVar convertedfilemap)
        cfmvar <- return (convertedfilemap ! filepath)
        cf <- liftIO $ readMVar cfmvar
        case cf of -- If the file was deleted, we need to rebuild.
          ConversionDeleted -> (getConvertedFile filepath)
          _                 -> return cfmvar
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
                          liftIO (putMVar convertedFilesMVar (Map.insert filepath cfmvar convertedfilemap))
                          return cfmvar
                       )

cleanupPeriodically :: Mp3fsM ()
cleanupPeriodically = forever $ do
                        cleanupOutdatedFiles
                        delay <- (liftM cleanupDelay) ask
                        liftIO $ threadDelay delay

cleanupOutdatedFiles :: Mp3fsM ()
cleanupOutdatedFiles = do
  convertedFilesMVar <- (liftM convertedFiles) ask
  convertedfilemap <- liftIO (takeMVar convertedFilesMVar)
  now <- liftIO $ getClockTime
  delay <- (liftM cleanupDelay) ask
  toDelete <- mapFilterM_ (staleFile now (toInteger delay) ) convertedfilemap
  liftIO $ putMVar convertedFilesMVar (Map.difference convertedfilemap toDelete)
  mapM deleteConvertedFile (Map.elems toDelete)
  return ()
    where
      staleFile now delay (path, cfmvar) = do
            cf <- liftIO $ readMVar cfmvar
            case cf of
              ConvertedFile { complete = True, lastAccess = t } -> return ((secDelayBetween t now) > delay)
              _                                                 -> return False

deleteConvertedFile :: MVar ConvertedFile -> Mp3fsM ()
deleteConvertedFile cfmvar = do
  cf <- liftIO $ takeMVar cfmvar
  td <- (liftM tempdir) ask
  if ((takeDirectory (convertedPath cf)) == td) -- Delete the file, but only if it is contained in our tempdir
     then liftIO $ removeLink (convertedPath cf)
     else return ()
  case cf of
    ConvertedFile { complete = True } -> (liftIO $ putMVar cfmvar ConversionDeleted)
    _                                 -> (liftIO $ putMVar cfmvar cf)
