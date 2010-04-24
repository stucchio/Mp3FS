module Mp3fsConverters
    (
      Mp3Converter (..)
    ) where

import Mp3fsInternal

import System.Directory
import System.IO
import System.FilePath.Posix
import System.Posix.Files
import System.Process
import Data.Map( fromList, member, empty, Map, (!), insert, keys)
import Control.Concurrent
import Control.Concurrent.MVar



data Mp3Converter = Mp3Converter {
                                  testIfActive :: () -> IO Bool,
                                  converterFunc :: Mp3fsInternalData -> FilePath -> IO ConvertedFile
                                  }

canFindExecutable prog () =
    do
      execLoc <- findExecutable prog
      case execLoc of
        Nothing -> return False
        Just s -> return True

convertMp3 :: FilePath -> Mp3fsM ConvertedFile
convertMp3 path =
    do
      handle <- openFile path ReadMode
      mvb <- newMVar True
      return ConvertedFile { handle = Just handle, name = path, complete = mvb, convertedPath = path }
    `catch`
    \e -> return ConversionFailure

mp3Converter = Mp3Converter { testIfActive = \() -> True,
                              converterFunc = convertMp3
                            }

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

wavConverter = Mp3Converter { testIfActive = canFindExecutable "lame",
                              converterFunc = convertWav
                            }



musicConverters = fromList [ (".ogg", convertOgg ), (".mp3", convertMp3 ), (".wav", convertWav) ]
musicExtensions = keys musicConverters
mp3FormatExtension = ".mp3"
