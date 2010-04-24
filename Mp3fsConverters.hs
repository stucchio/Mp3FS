module Mp3fsConverters
    (
     getMp3Converters
    ) where

import Mp3fsInternal

import System.Directory
import System.IO
import System.FilePath.Posix
import System.Posix.Files
import System.Process
import Data.Map( fromList, member, empty, Map, (!), insert, keys)
import Control.Concurrent
import Control.Monad.Reader
import Control.Concurrent.MVar



data Mp3Converter = Mp3Converter {
                                  testIfActive :: () -> IO Bool,
                                  converterFunc :: FilePath -> Mp3fsM ConvertedFile
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
      handle <- mp3OpenFile path
      case handle of
        Nothing -> return ConversionFailure
        Just h -> do
          mvb <- liftIO (newMVar True)
          return ConvertedFile { handle = Just h, name = path, complete = mvb, convertedPath = path }


mp3Converter = Mp3Converter { testIfActive = \() -> return True,
                              converterFunc = convertMp3
                            }

makeConverter convertFile path =
    do
      (finalPath, finalHandle) <- mp3GetTempFile
      mvb <- liftIO newEmptyMVar
      liftIO (forkIO (convertFile path finalPath finalHandle mvb ))
      return ConvertedFile { name=path, handle = Just finalHandle, convertedPath = finalPath, complete = mvb}

{-
convertOgg uses oggdec and lame to convert an ogg to mp3. Roughly equivalent to the following shell commands:

oggdec file.ogg
lame --preset 192 -ms -h file.wav
-}
convertOgg :: FilePath -> Mp3fsM ConvertedFile
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

mp3Converter = Mp3Converter { testIfActive = \() -> filterM [canFindExereturn True,
                              converterFunc = convertMp3
                            }


convertWav :: FilePath -> Mp3fsM ConvertedFile
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

getMp3Converters