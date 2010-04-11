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
  config <- return $ Mp3fsConfig {rootdir = rootdir, builtFiles = empty }
  internal <- initInternalData ()
  withArgs (tail args) (fuseMain (mp3fsOps config internal) defaultExceptionHandler)

data Mp3fsConfig = Mp3fsConfig {
                                rootdir :: FilePath,
                                builtFiles :: Map FilePath FilePath
                               } deriving (Show)



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
                                            convertedFiles :: MVar (Map FilePath ConvertedFile ),
                                            tempdir :: FilePath,
                                            tempfilecount :: MVar Int
                                            }

initInternalData :: () -> IO Mp3fsInternalData
initInternalData () = do
  convfiles <- newMVar (fromList [])
  mainTempDir <- getTemporaryDirectory
  dir <- mkdtemp (combine mainTempDir "mp3fsXXXXXX")
  count <- newMVar 0
  return Mp3fsInternalData { convertedFiles = convfiles,
                             tempdir = dir,
                             tempfilecount = count
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

mp3fsOps :: Mp3fsConfig -> Mp3fsInternalData -> FuseOperations HT
mp3fsOps config internal = defaultFuseOps { fuseGetFileStat = mp3GetFileStat config internal
                                            , fuseRead        = mp3Read config internal
                                            , fuseOpen        = mp3OpenFile config internal
                                            , fuseOpenDirectory = mp3OpenDirectory
                                            , fuseReadDirectory = \x -> runReaderT (mp3ReadDirectory x) config
                                            , fuseGetFileSystemStats = helloGetFileSystemStats
                                            , fuseDestroy = mp3Destroy config internal
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

mp3OpenFile ::  Mp3fsConfig -> Mp3fsInternalData -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
mp3OpenFile _ _ path WriteOnly _ = return (Left ePERM)
mp3OpenFile _ _ _    ReadWrite _ = return (Left ePERM)
mp3OpenFile config internal path ReadOnly flags  = do
  convertedFile <- getConvertedFile config internal path
  putStrLn ("Opened file " ++ path ++ (show convertedFile) )
  ecode <- (exitCode convertedFile)
  return ecode
    where
      exitCode ConversionFailure = return (Left eNOENT)
      exitCode cf = (getConvertedHandle cf) >> return (Right ());

convertedFileStat ConversionFailure = return (Left eNOENT)
convertedFileStat ConvertedFile { convertedPath = cp, complete = c} =
    do
      readMVar c
      exists <- fileExist cp
      if exists
         then
             do
               status <- getFileStatus cp
               return (Right (fileStatusToFileStat status))
         else
             return (Left eNOENT)

mp3GetFileStat :: Mp3fsConfig -> Mp3fsInternalData -> FilePath -> IO (Either Errno FileStat)
mp3GetFileStat config internal path =
    do
      isdir <- doesDirectoryExist pathToFile
      if isdir
         then
             do
               status <- getFileStatus pathToFile
               return (Right (fileStatusToFileStat status) )
         else
             do
               convFile <- getConvertedFile config internal path
               stat <- convertedFileStat convFile
               return stat
    where
      pathToFile = (makeAbsPathRelativeToRoot config path)


mp3OpenDirectory _ = return eOK

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

makeAbsPathRelativeToRoot config path = (combine root (makeRelative "/" path) )
                                        where
                                          root = (rootdir config)

convertMp3 :: Mp3fsConfig -> Mp3fsInternalData -> FilePath -> IO ConvertedFile
convertMp3 config internal path =
    do
      handle <- openFile path ReadMode
      mvb <- newMVar True
      return ConvertedFile { handle = Just handle, name = path, complete = mvb, convertedPath = path }
    `catch`
    \e -> return ConversionFailure

{-
convertOgg uses oggdec and lame to convert an ogg to mp3. Roughly equivalent to the following shell commands:

oggdec file.ogg
lame --preset 192 -ms -h file.wav
-}
convertOgg :: Mp3fsConfig -> Mp3fsInternalData -> FilePath -> IO ConvertedFile
convertOgg config internal path =
    do
      nextFileCount <- getNextTempCount internal
      (finalPath, finalHandle) <- openTempFile td (((dropExtension . takeFileName) path ) ++ ".mp3")
      mvb <- newEmptyMVar
      forkIO (convertFile path finalPath finalHandle mvb )
      return ConvertedFile { name=path, handle = Just finalHandle, convertedPath = finalPath, complete = mvb}
    where
      td = tempdir internal
      pipeName i = addExtension (combine td ("conversionPipe" ++ ( show i))) ".wav"
      convertFile basefile finalpath finalHandle mvb =
          do
            createNamedPipe wavPath (unionFileModes ownerReadMode ownerWriteMode)
            forkIO ((system ("oggdec " ++ basefile ++ " -o " ++ wavPath)) >> return ())
            system ("lame  " ++ wavPath ++ " " ++ finalpath)
            removeLink (replaceExtension finalpath ".wav")
            hSeek finalHandle AbsoluteSeek 0
            putMVar mvb True
            return ()
          where
            wavPath = replaceExtension finalpath ".wav"

musicConverters = fromList [ (".ogg", convertOgg ), (".mp3", convertMp3 ) ]
musicExtensions = keys musicConverters
mp3FormatExtension = ".mp3"

getConvertedFile config internal filepath =
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
                         convertedFile <- (musicConverters ! (takeExtension (head pathsToCheck))) config internal (head pathsToCheck)
                         putbackfilemap (insert filepath convertedFile convertedfilemap)
                         return convertedFile
    where
      putbackfilemap = putMVar (convertedFiles internal)
      possiblePaths = map (\x -> replaceExtension ((makeAbsPathRelativeToRoot config) filepath) x) musicExtensions


isMusicFileOrDir :: FilePath -> IO Bool
isMusicFileOrDir x = (liftM2 (||)) (isMusicFile x) (isFilePathDirectory x)

isMusicFile :: FilePath -> IO Bool
isMusicFile x = (liftM (&& (member ext musicConverters)) ) (doesFileExist x)
    where ext = (takeExtension x)

isFilePathDirectory :: FilePath -> IO Bool
isFilePathDirectory x = doesDirectoryExist x

filterMusicFiles :: FilePath -> [FilePath] -> IO [FilePath]
filterMusicFiles rootdir filenames = filterM (\file -> isMusicFile (combine rootdir file)) filenames

mp3ReadDirectory :: FilePath -> ReaderT Mp3fsConfig IO (Either Errno [(FilePath, FileStat)])
mp3ReadDirectory path = do
  config <- ask
  root <- (liftM rootdir) ask
  ctx <- liftIO $ getFuseContext
  basePathToRead <- return $ makeAbsPathRelativeToRoot config path
  baseDirectoryContents <- liftIO $ (getDirectoryContents basePathToRead )
  musicContents <- liftIO $ filterMusicFiles basePathToRead baseDirectoryContents
  dirContents <- liftIO $ filterM (\x -> isFilePathDirectory (combine root x)) baseDirectoryContents
  musicStatus <- liftIO $ (sequence (map musicFileStatus (addBasePath basePathToRead musicContents)))
  dirStatus <- liftIO $ (sequence (map dirFileStatus (addBasePath basePathToRead dirContents)))
  return $ Right (musicStatus ++ dirStatus ++ [(".",          dirStat  ctx), ("..",          dirStat  ctx)])
      where
        addBasePath bp = (map (combine bp))
        dirFileStatus x = getFileStatus x >>= \s -> return ( takeFileName x, fileStatusToFileStat s)
        musicFileStatus x = getFileStatus x >>= \s -> return (replaceExtension (takeFileName x) mp3FormatExtension, fileStatusToFileStat s )


mp3Read :: Mp3fsConfig -> Mp3fsInternalData -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
mp3Read config internal path _ byteCount offset =
    do
      convFile <- getConvertedFile config internal path
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
      basePath = makeAbsPathRelativeToRoot config path

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

mp3Destroy config internal =
    do
      removeRecursiveSafely (tempdir internal)
