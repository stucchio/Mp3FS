module Main () where

import Mp3fsInternal
import Mp3fsConverters

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.Unix.Directory (mkdtemp, removeRecursiveSafely)
import System.IO
import System.FilePath.Posix
import System.Environment
import System.Directory
import System.Fuse
import System.Process
import Data.Map( fromList, member, empty, Map, (!), insert, keys)
import Data.ByteString (hGet)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

type HT = ()

main :: IO ()
main = do
  args <- getArgs
  rootdir <- getAbsoluteRoot (head args)
  converters <- getMp3Converters
  internal <- (initInternalData rootdir (fromList converters))
  loc <- findExecutable "lame"
  forkIO (runMp3fsM cleanupPeriodically internal)
  withArgs (tail args) (fuseMain (mp3fsOps internal) defaultExceptionHandler)

getAbsoluteRoot :: FilePath -> IO FilePath
getAbsoluteRoot rootdir = -- Turns the rootdir into an absolute path. If rootdir is a relpath, prepends the current working directory.
    if (isAbsolute rootdir) then
       return $ rootdir
    else
        do
          pwd <- getEnv "PWD"
          return $ (combine pwd rootdir)

mp3fsOps :: Mp3fsInternalData -> FuseOperations HT
mp3fsOps internal = defaultFuseOps { fuseGetFileStat   = runMp3fsM1 mp3GetFileStat internal
                                   , fuseRead          = runMp3fsM4 mp3Read internal
                                   , fuseOpen          = runMp3fsM3 mp3OpenFile internal
                                   , fuseRelease       = runMp3fsM2 mp3ReleaseFile internal
                                   , fuseOpenDirectory = runMp3fsM1 mp3OpenDirectory internal
                                   , fuseReadDirectory = runMp3fsM1 mp3ReadDirectory internal
                                   , fuseDestroy       = runMp3fsM mp3Destroy internal
                                   , fuseGetFileSystemStats = mp3GetFileSystemStats
                                   }

mp3OpenFile :: FilePath -> OpenMode -> OpenFileFlags -> Mp3fsM (Either Errno HT)
mp3OpenFile path WriteOnly _ = return (Left ePERM)
mp3OpenFile _    ReadWrite _ = return (Left ePERM)
mp3OpenFile path ReadOnly flags  = modifyConvertedFile path openFileWorker
    where
      openFileWorker ConversionFailure = return ((Left eNOENT), ConversionFailure)
      openFileWorker cf = do
        cfNew <- incReaders cf
        return (Right (), cfNew)

mp3ReleaseFile path _ = modifyConvertedFile path (\cf -> (decReaders cf) >>=
                                                         (\cfnew -> return ((), cf)))

whenFileExists filename ifExist ifNotExist =
    do
      exists <- fileExist filename
      if exists
         then ifExist
         else return ifNotExist

convertedFileStat :: ConvertedFile -> IO (Either Errno FileStat)
convertedFileStat ConversionFailure = return (Left eNOENT)
convertedFileStat FileDoesNotExist = return (Left eNOENT)
convertedFileStat ConvertedFile { convertedPath = cp, complete = False } = return (Left eINPROGRESS)
convertedFileStat ConvertedFile { convertedPath = cp, complete = True } =
    do
      whenFileExists cp statusOfExistingFile (Left eNOENT)
    where
      statusOfExistingFile = getFileStatus cp >>= \status -> return (Right (fileStatusToFileStat status))

mp3GetFileStat :: FilePath -> Mp3fsM (Either Errno FileStat)
mp3GetFileStat path =
    do
      pathToFile <- makeAbsPathRelativeToRoot path
      isdir <- liftIO $ doesDirectoryExist pathToFile
      if isdir
         then
             do
               status <- liftIO $ getFileStatus pathToFile
               return (Right (fileStatusToFileStat status) )
         else withConvertedFile path (\cf -> liftIO $ convertedFileStat cf)

mp3OpenDirectory :: FilePath -> Mp3fsM Errno
mp3OpenDirectory path =
    do
      basePathToRead <- makeAbsPathRelativeToRoot path
      isDir <- liftIO (doesDirectoryExist basePathToRead)
      if isDir then (return eOK) else (return eNOENT)

fileStatusToEntryType :: FileStatus -> EntryType
fileStatusToEntryType status
    | isSymbolicLink    status = SymbolicLink
    | isNamedPipe       status = NamedPipe
    | isCharacterDevice status = CharacterSpecial
    | isDirectory       status = Directory
    | isBlockDevice     status = BlockSpecial
    | isRegularFile     status = RegularFile
    | isSocket          status = Socket
    | otherwise                = Unknown

fileStatusToFileStat :: FileStatus -> FileStat
fileStatusToFileStat status =
    FileStat { statEntryType        = fileStatusToEntryType status
             , statFileMode         = fileMode status
             , statLinkCount        = linkCount status
             , statFileOwner        = fileOwner status
             , statFileGroup        = fileGroup status
             , statSpecialDeviceID  = specialDeviceID status
             , statFileSize         = fileSize status
             -- fixme: 1024 is not always the size of a block
             , statBlocks           = fromIntegral (fileSize status `div` 1024)
             , statAccessTime       = accessTime status
             , statModificationTime = modificationTime status
             , statStatusChangeTime = statusChangeTime status
             }

mp3ReadDirectory :: FilePath -> Mp3fsM (Either Errno [(FilePath, FileStat)])
mp3ReadDirectory path = do
  basePathToRead <- makeAbsPathRelativeToRoot path
  exists <- liftIO (doesDirectoryExist basePathToRead)
  if exists
     then
         do
           ctx <- liftIO $ getFuseContext
           baseDirectoryContents <- liftIO $ (getDirectoryContents basePathToRead )
           musicContents <- mp3FilterMusicFiles baseDirectoryContents
           dirContents <- liftIO $ filterM (\x -> doesDirectoryExist (combine basePathToRead x)) baseDirectoryContents
           musicStatus <- liftIO $ (sequence (map musicFileStatus (addBasePath basePathToRead musicContents)))
           dirStatus <- liftIO $ (sequence (map dirFileStatus (addBasePath basePathToRead dirContents)))
           return (Right (musicStatus ++ dirStatus ))
     else
         return (Left eNOENT)
    where
      addBasePath bp = (map (combine bp))
      dirFileStatus x = getFileStatus x >>= \s -> return ( takeFileName x, fileStatusToFileStat s)
      musicFileStatus x = getFileStatus x >>= \s -> return (replaceExtension (takeFileName x) mp3FormatExtension, fileStatusToFileStat s )

mp3Read :: FilePath -> HT -> ByteCount -> FileOffset -> Mp3fsM (Either Errno B.ByteString)
mp3Read path _ byteCount offset = withConvertedFile path readFile
    where
      readFile convFile = if ((takeExtension path) == ".mp3")
                            then do
                                  handle <- getConvertedHandle convFile
                                  seek <- liftIO (hSeek handle AbsoluteSeek (toInteger offset))
                                  bytes <- liftIO (hGet handle (fromIntegral (toInteger byteCount)))
                                  return (Right bytes)
                            else return (Left eNOENT)

mp3GetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
mp3GetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

mp3Destroy =
    do
      td <- (liftM tempdir) ask
      liftIO (removeRecursiveSafely td)

