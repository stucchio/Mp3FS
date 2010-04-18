module Main where

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
import Control.Monad.Reader


import Control.Monad

type HT = ()

main :: IO ()
main = do
  args <- getArgs
  rootdir <- getAbsoluteRoot (head args)
  internal <- initInternalData rootdir
  withArgs (tail args) (fuseMain (mp3fsOps internal) defaultExceptionHandler)

data ConvertedFile = ConvertedFile { -- The data type of a file which we have
                                     name :: FilePath,
                                     convertedPath :: FilePath,
                                     handle :: Maybe Handle,
                                     complete :: MVar Bool
                                   } | ConversionFailure | FileDoesNotExist


getConvertedHandle ConvertedFile { handle = Just h, complete = c} = (readMVar c) >> return h
getConvertedHandle ConvertedFile { handle = Nothing, convertedPath = cp, complete = c } = (readMVar c) >> openFile cp ReadMode


instance Show ConvertedFile where
    show ConversionFailure = "ConversionFailure"
    show FileDoesNotExist = "FileDoesNotExist"
    show ConvertedFile { name = nm, handle = Nothing } = "Converted file: { name = " ++ nm ++ ", no handle}"
    show ConvertedFile { name = nm, handle = Just h } = "Converted file: { name = " ++ nm ++ ", has handle}"

data Mp3fsInternalData = Mp3fsInternalData {
                                            rootdir :: FilePath,
                                            convertedFiles :: MVar (Map FilePath ConvertedFile ),
                                            tempdir :: FilePath,
                                            tempfilecount :: MVar Int
                                            }

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


getAbsoluteRoot :: FilePath -> IO FilePath
getAbsoluteRoot rootdir = -- Turns the rootdir into an absolute path. If rootdir is a relpath, prepends the current working directory.
    if (isAbsolute rootdir) then
       return $ rootdir
    else
        do
          pwd <- getEnv "PWD"
          return $ (combine pwd rootdir)

runReaderT1 f r = \x -> runReaderT (f x) r
runReaderT2 f r = \x -> \y -> runReaderT (f x y) r
runReaderT3 f r = \x -> \y -> \z -> runReaderT (f x y z) r
runReaderT4 f r = \x -> \y -> \z -> \t -> runReaderT (f x y z t) r

mp3fsOps :: Mp3fsInternalData -> FuseOperations HT
mp3fsOps internal = defaultFuseOps { fuseGetFileStat = runReaderT1 mp3GetFileStat internal
                                   , fuseRead        = mp3Read internal
                                   , fuseOpen        = runReaderT3 mp3OpenFile internal
                                   , fuseOpenDirectory = runReaderT1 mp3OpenDirectory internal
                                   , fuseReadDirectory = \x -> runReaderT (mp3ReadDirectory x) internal
                                   , fuseGetFileSystemStats = helloGetFileSystemStats
                                   , fuseDestroy = mp3Destroy internal
                                   }

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"
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

convertMp3 :: Mp3fsInternalData -> FilePath -> IO ConvertedFile
convertMp3 internal path =
    do
      handle <- openFile path ReadMode
      mvb <- newMVar True
      return ConvertedFile { handle = Just handle, name = path, complete = mvb, convertedPath = path }
    `catch`
    \e -> return ConversionFailure


makeConverter convertFile internal path =
    do
      nextFileCount <- getNextTempCount internal
      (finalPath, finalHandle) <- openTempFile td (((dropExtension . takeFileName) path ) ++ ".mp3")
      mvb <- newEmptyMVar
      forkIO (convertFile path finalPath finalHandle mvb )
      return ConvertedFile { name=path, handle = Just finalHandle, convertedPath = finalPath, complete = mvb}
    where
      td = tempdir internal

{-
convertOgg uses oggdec and lame to convert an ogg to mp3. Roughly equivalent to the following shell commands:

oggdec file.ogg
lame --preset 192 -ms -h file.wav
-}
convertOgg :: Mp3fsInternalData -> FilePath -> IO ConvertedFile
convertOgg = makeConverter convertFile
    where
      convertFile basefile finalpath finalHandle mvb =
          do
            createNamedPipe wavPath (unionFileModes ownerReadMode ownerWriteMode)
            forkIO ((system ("oggdec " ++ basefile ++ " -o " ++ wavPath)) >> return ())
            system ("lame  " ++ wavPath ++ " " ++ finalpath)
            removeLink wavPath
            hSeek finalHandle AbsoluteSeek 0
            putMVar mvb True
            return ()
          where
            wavPath = replaceExtension finalpath ".wav"

convertWav :: Mp3fsInternalData -> FilePath -> IO ConvertedFile
convertWav = makeConverter convertFile
    where
      convertFile basefile finalpath finalHandle mvb =
          do
            system ("lame  " ++ basefile ++ " " ++ finalpath)
            hSeek finalHandle AbsoluteSeek 0
            putMVar mvb True
            return ()

musicConverters = fromList [ (".ogg", convertOgg ), (".mp3", convertMp3 ), (".wav", convertWav) ]
musicExtensions = keys musicConverters
mp3FormatExtension = ".mp3"

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

mp3Destroy internal =
    do
      removeRecursiveSafely (tempdir internal)
