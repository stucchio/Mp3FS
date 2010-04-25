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
  internal <- initInternalData rootdir
  loc <- findExecutable "lame"
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
mp3fsOps internal = defaultFuseOps { fuseGetFileStat = runMp3fsM1 mp3GetFileStat internal
                                   , fuseRead        = mp3Read internal
                                   , fuseOpen        = runMp3fsM3 mp3OpenFile internal
                                   , fuseOpenDirectory = runMp3fsM1 mp3OpenDirectory internal
                                   , fuseReadDirectory = \x -> runMp3fsM (mp3ReadDirectory x) internal
                                   , fuseGetFileSystemStats = helloGetFileSystemStats
                                   , fuseDestroy = runMp3fsM1 mp3Destroy internal
                                   }

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }


fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length helloString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }


mp3OpenFile :: FilePath -> OpenMode -> OpenFileFlags -> ReaderT Mp3fsInternalData IO (Either Errno HT)
mp3OpenFile path WriteOnly _ = return (Left ePERM)
mp3OpenFile _    ReadWrite _ = return (Left ePERM)
mp3OpenFile path ReadOnly flags  = do
  internal <- ask
  convertedFile <- getConvertedFileR path
  ecode <- liftIO (exitCode convertedFile)
  return ecode
    where
      exitCode ConversionFailure = return (Left eNOENT)
      exitCode cf = (getConvertedHandle cf) >> return (Right ());

whenFileExists filename ifExist ifNotExist =
    do
      exists <- fileExist filename
      if exists
         then ifExist
         else return ifNotExist

convertedFileStat ConversionFailure = return (Left eNOENT)
convertedFileStat FileDoesNotExist = return (Left eNOENT)
convertedFileStat ConvertedFile { convertedPath = cp, complete = c} =
    do
      readMVar c
      whenFileExists cp statusOfExistingFile (Left eNOENT)
    where
      statusOfExistingFile = getFileStatus cp >>= \status -> return (Right (fileStatusToFileStat status))

mp3GetFileStat :: FilePath -> ReaderT Mp3fsInternalData IO (Either Errno FileStat)
mp3GetFileStat path =
    do
      pathToFile <- makeAbsPathRelativeToRootR path
      isdir <- liftIO (doesDirectoryExist pathToFile)
      if isdir
         then
             do
               status <- liftIO (getFileStatus pathToFile)
               return (Right (fileStatusToFileStat status) )
         else
             do
               convFile <- getConvertedFileR path
               stat <- liftIO (convertedFileStat convFile)
               return stat

mp3OpenDirectory :: FilePath -> ReaderT Mp3fsInternalData IO Errno
mp3OpenDirectory path =
    do
      basePathToRead <- makeAbsPathRelativeToRootR path
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

prependRootDirToList rootdir lst = map (combine rootdir) lst

makeAbsPathRelativeToRootR path = ((liftM rootdir) ask) >>= \root -> return (combine root (makeRelative "/" path) )

makeAbsPathRelativeToRoot internal path = (combine root (makeRelative "/" path) )
    where
      root = (rootdir internal)


getConvertedFile internal filepath =
    do
      convertedfilemap <- takeMVar (convertedFiles internal)
      if (member filepath convertedfilemap)
          then do
                putbackfilemap convertedfilemap
                return (convertedfilemap ! filepath)
          else do
                pathsToCheck <- filterM fileExist possiblePaths
                if pathsToCheck == []
                   then do
                         putbackfilemap convertedfilemap
                         return FileDoesNotExist
                   else
                       do
                         convertedFile <- (musicConverters ! (takeExtension (head pathsToCheck))) internal (head pathsToCheck)
                         putbackfilemap (insert filepath convertedFile convertedfilemap)
                         return convertedFile
    where
      putbackfilemap = putMVar (convertedFiles internal)
      possiblePaths = map (\x -> replaceExtension ((makeAbsPathRelativeToRoot internal) filepath) x) musicExtensions

getConvertedFileR :: FilePath -> ReaderT Mp3fsInternalData IO ConvertedFile
getConvertedFileR filepath =
    do
      convertedFilesMVar <- (liftM convertedFiles) ask
      convertedfilemap <- liftIO (takeMVar convertedFilesMVar)
      if (member filepath convertedfilemap)
          then do
                liftIO (putMVar convertedFilesMVar convertedfilemap)
                return (convertedfilemap ! filepath)
          else do
                absFilePath <- makeAbsPathRelativeToRootR filepath
                pathsToCheck <- liftIO (filterM fileExist (map (\x -> replaceExtension absFilePath x) musicExtensions))
                if pathsToCheck == []
                   then do
                         liftIO (putMVar convertedFilesMVar convertedfilemap)
                         return FileDoesNotExist
                   else
                       do
                         internal <- ask
                         convertedFile <- liftIO ((musicConverters ! (takeExtension (head pathsToCheck))) internal (head pathsToCheck))
                         liftIO (putMVar convertedFilesMVar (insert filepath convertedFile convertedfilemap))
                         return convertedFile




isMusicFileOrDir :: FilePath -> IO Bool
isMusicFileOrDir x = (liftM2 (||)) (isMusicFile x) (isFilePathDirectory x)

isMusicFile :: FilePath -> IO Bool
isMusicFile x = (liftM (&& (member ext musicConverters)) ) (doesFileExist x)
    where ext = (takeExtension x)

isFilePathDirectory :: FilePath -> IO Bool
isFilePathDirectory x = doesDirectoryExist x

filterMusicFiles :: FilePath -> [FilePath] -> IO [FilePath]
filterMusicFiles rootdir filenames = filterM (\file -> isMusicFile (combine rootdir file)) filenames

mp3ReadDirectory :: FilePath -> ReaderT Mp3fsInternalData IO (Either Errno [(FilePath, FileStat)])
mp3ReadDirectory path = do
  basePathToRead <- makeAbsPathRelativeToRootR path
  exists <- liftIO (doesDirectoryExist basePathToRead)
  if exists
     then
         do
           root <- (liftM rootdir) ask
           ctx <- liftIO $ getFuseContext
           baseDirectoryContents <- liftIO $ (getDirectoryContents basePathToRead )
           musicContents <- liftIO $ filterMusicFiles basePathToRead baseDirectoryContents
           dirContents <- liftIO $ filterM (\x -> isFilePathDirectory (combine root x)) baseDirectoryContents
           musicStatus <- liftIO $ (sequence (map musicFileStatus (addBasePath basePathToRead musicContents)))
           dirStatus <- liftIO $ (sequence (map dirFileStatus (addBasePath basePathToRead dirContents)))
           return (Right (musicStatus ++ dirStatus ++ [(".",          dirStat  ctx), ("..",          dirStat  ctx)]))
     else
         return (Left eNOENT)
    where
      addBasePath bp = (map (combine bp))
      dirFileStatus x = getFileStatus x >>= \s -> return ( takeFileName x, fileStatusToFileStat s)
      musicFileStatus x = getFileStatus x >>= \s -> return (replaceExtension (takeFileName x) mp3FormatExtension, fileStatusToFileStat s )



mp3Read :: Mp3fsInternalData -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
mp3Read internal path _ byteCount offset =
    do
      convFile <- getConvertedFile internal path
      if ((takeExtension basePath) == mp3FormatExtension)
        then do
              putStrLn ("          READING FROM " ++ path)
              handle <- getConvertedHandle convFile
              putStrLn ("          Got HANDLE " ++ path)
              seek <- hSeek handle AbsoluteSeek (toInteger offset)
              bytes <- hGet handle (fromIntegral (toInteger byteCount))
              return $ Right bytes
        else return $ Left eNOENT
    where
      basePath = makeAbsPathRelativeToRoot internal path

--THIS IS NOT REMOTELY FINISHED
{-
mp3GetConvertedFile :: Mp3fsInternalData -> FilePath -> IO Maybe ConvertedFile
mp3GetConvertedFile config internal path =
    do
      datamap <- readMVar filemapMVar
      cfile <- lookup path datamap
      return cfile
          where filemapMVar = convertedFiles internal
                basePath = makeAbsPathRelativeToRoot config path
                convertFileIfNecessary Just f = return f
                convertFileIfNecessary Nothing =
                    do
                      map <- takeMVar filemapMVar
                      cfile <- (lookup ext musicConverters) config path
                          where
                            ext = takeExtension path
-}


helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
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

