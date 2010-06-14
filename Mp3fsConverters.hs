module Mp3fsConverters
    (
     getMp3Converters
    , mp3FormatExtension
    ) where

import Mp3fsInternal

import System.Directory
import System.IO
import System.Time
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
                                  converterFunc :: FilePath -> Mp3fsM (MVar ConvertedFile)
                                  }

canFindExecutable prog = do
  execLoc <- findExecutable prog
  case execLoc of
    Nothing -> return False
    Just s -> return True

convertMp3 :: FilePath -> Mp3fsM (MVar ConvertedFile)
convertMp3 path = do
  handle <- getFileHandleOrNothing path
  cf <- case handle of
          Nothing -> return ConversionFailure
          Just h -> (newConvertedFile path path handle True)
  cfmvar <- liftIO $ newMVar cf
  return cfmvar

mp3Converter = Mp3Converter { ext = ".mp3",
                              testIfActive = \() -> return True,
                              converterFunc = convertMp3
                            }

{- makeConverter takes a conversion function and
-}
makeConverter :: (FilePath -> Handle -> MVar ConvertedFile -> Mp3fsM ()) -> (FilePath -> Mp3fsM (MVar ConvertedFile))
makeConverter convertFile path = do
  (finalPath, finalHandle) <- mp3GetTempFile (replaceExtension path ".mp3")
  cf <- (newConvertedFile path finalPath (Just finalHandle) False)
  cfmvar <- liftIO $ newMVar cf
  internal <- ask
  liftIO (forkOS ((runMp3fsM3 convertFile internal) path finalHandle cfmvar))
  return cfmvar
    where
      quoteForShell s = "\"" ++ s ++ "\""

{-
convertViaWave is a general function.

It takes one argument via command line (a function mapping input files to output files), which tells it how to convert a
music file to a wav file. It then uses lame to convert the wave file to mp3.
-}
convertViaWav toWavCmdLine = makeConverter convertFile
    where
      convertFile basefile handle cfmvar =
          do
            wavPath <- mp3GetTempPipe
            liftIO (do
                      (forkIO ((system (toWavCmdLine basefile wavPath)) >> return ()))
                      cf <- readMVar cfmvar
                      system ("lame  " ++ wavPath ++ " " ++ (enquote (convertedPath cf)))
                      removeLink wavPath
                      hSeek handle AbsoluteSeek 0
                      time <- liftIO $ getClockTime
                      modifyMVar_ cfmvar (\cf -> return (cf { complete = True, lastAccess = time }))
                   )
            return ()

enquote path = "\"" ++ path ++ "\""

{-
convertOgg uses oggdec and lame to convert an ogg to mp3. Roughly equivalent to the following shell commands:

oggdec file.ogg
lame --preset 192 -ms -h file.wav
-}
oggConverter = Mp3Converter { ext = ".ogg",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "oggdec")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("oggdec " ++ (enquote basefile) ++ " -o " ++ (enquote wavpath)))
                            }



flacConverter = Mp3Converter { ext = ".flac",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "flac")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("flac -cd " ++ (enquote basefile) ++ " > " ++ (enquote wavpath)))
                            }

aacConverter = Mp3Converter { ext = ".aac",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "faad")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("faad " ++ basefile ++ " -o " ++ (enquote wavpath)))
                            }

mp4Converter = Mp3Converter { ext = ".mp4",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "faad")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("faad " ++ basefile ++ " -o " ++ (enquote wavpath)))
                            }
wmaConverter = Mp3Converter { ext = ".wma",
                              testIfActive = \() -> ((liftM2 (&&)) (canFindExecutable "lame") (canFindExecutable "ffmpeg")),
                              converterFunc = convertViaWav (\basefile -> \wavpath -> ("ffmpeg " ++ basefile ++" -aq " ++ basefile) )
                            }


convertWav :: FilePath -> Mp3fsM (MVar ConvertedFile)
convertWav = makeConverter convertFile
    where
      convertFile path handle cfmvar = liftIO (do
                                                 cf <- readMVar cfmvar
                                                 system ("lame  " ++ (enquote path) ++ " " ++ (enquote (convertedPath cf)))
                                                 hSeek handle AbsoluteSeek 0
                                                 modifyMVar_ cfmvar (\cf -> return (cf { complete = True }))
                                                 return ()
                                              )

wavConverter = Mp3Converter { ext = ".wav",
                              testIfActive = \() -> canFindExecutable "lame",
                              converterFunc = convertWav
                            }

musicConverters = [wavConverter, oggConverter, mp3Converter, flacConverter, mp4Converter, aacConverter]
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
