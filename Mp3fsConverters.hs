module Mp3fsConverters
    (
     getMp3Converters
    , mp3FormatExtension
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
                                  ext :: String,
                                  testIfActive :: () -> IO Bool,
                                  converterFunc :: FilePath -> Mp3fsM ConvertedFile
                                  }

canFindExecutable prog =
    do
      execLoc <- findExecutable prog
      case execLoc of
        Nothing -> return False
        Just s -> return True

convertMp3 :: FilePath -> Mp3fsM ConvertedFile
convertMp3 path =
    do
      handle <- getFileHandleOrNothing path
      case handle of
        Nothing -> return ConversionFailure
        Just h -> do
          mvb <- liftIO (newMVar True)
          return ConvertedFile { handle = Just h, name = path, complete = mvb, convertedPath = path }


mp3Converter = Mp3Converter { ext = ".mp3",
                              testIfActive = \() -> return True,
                              converterFunc = convertMp3
                            }

makeConverter convertFile path =
    do
      (finalPath, finalHandle) <- mp3GetTempFile (replaceExtension path ".mp3")
      mvb <- liftIO newEmptyMVar
      internal <- ask
      liftIO (forkIO ((runMp3fsM4 convertFile internal) (quoteForShell path) (quoteForShell finalPath) finalHandle mvb ))
      return ConvertedFile { name=path, handle = Just finalHandle, convertedPath = finalPath, complete = mvb}
    where
      quoteForShell s = "\"" ++ s ++ "\""

{-
convertViaWave is a general function.

It takes one argument via command line (a function mapping input files to output files), which tells it how to convert a
music file to a wav file. It then uses lame to convert the wave file to mp3.
-}
convertViaWav toWavCmdLine = makeConverter convertFile
    where
      convertFile basefile finalpath finalHandle mvb =
          do
            wavPath <- mp3GetTempPipe
            liftIO (do
                      (forkIO ((system (toWavCmdLine basefile wavPath)) >> return ()))
                      system ("lame  " ++ wavPath ++ " " ++ finalpath)
                      removeLink wavPath
                      hSeek finalHandle AbsoluteSeek 0
                      putMVar mvb True
                   )
            return ()


{-
convertOgg uses oggdec and lame to convert an ogg to mp3. Roughly equivalent to the following shell commands:

oggdec file.ogg
lame --preset 192 -ms -h file.wav
-}
oggConverter = Mp3Converter { ext = ".ogg",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "oggdec")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("oggdec " ++ basefile ++ " -o " ++ wavpath))
                            }



flacConverter = Mp3Converter { ext = ".flac",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "flac")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("flac -cd " ++ basefile ++ " > " ++ wavpath))
                            }

convertWav :: FilePath -> Mp3fsM ConvertedFile
convertWav = makeConverter convertFile
    where
      convertFile basefile finalpath finalHandle mvb = liftIO
                                                       (do
                                                          system ("lame  " ++ basefile ++ " " ++ finalpath)
                                                          hSeek finalHandle AbsoluteSeek 0
                                                          putMVar mvb True
                                                          return ()
                                                       )

wavConverter = Mp3Converter { ext = ".wav",
                              testIfActive = \() -> canFindExecutable "lame",
                              converterFunc = convertWav
                            }



musicConverters = [wavConverter, oggConverter, mp3Converter, flacConverter]
mp3FormatExtension = ".mp3"

getMp3Converters = (filterM (\x -> ((testIfActive x) ()) ) musicConverters) >>= (\x -> (mapM converterToExtFuncPair x))
    where
      converterToExtFuncPair conv = return (ext conv, converterFunc conv)

getFileHandleOrNothing :: FilePath -> Mp3fsM (Maybe Handle)
getFileHandleOrNothing path = (liftIO getHandle)
    where
      getHandle =
          do
            h <- (openFile path ReadMode)
            return (Just h)
          `catch`
          \e -> return Nothing
