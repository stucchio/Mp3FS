module Mp3fsConverters
    (
      Mp3Converter(..)
    ) where

import Mp3fsInternal
import System.Directory



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
