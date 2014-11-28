{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (concatMap)

import Control.Monad

import Data.Foldable hiding (mapM_)

import System.IO
import System.Console.CmdArgs
import System.Directory
import System.Directory.Tree hiding (name)
import System.FilePath
import System.Process

data MTLRArgs = MTLRTrain { regConst1 :: Integer
                          , regConst2 :: Integer
                          , input :: FilePath
                          , output :: FilePath
                          , timePoints :: Integer
                          , weight :: FilePath
                          , uncensored :: Bool
                          , intervalFile :: FilePath
                          }
                          
              | MTLRTest { input :: FilePath
                         , output :: FilePath
                         , timePoints :: Integer
                         , survivalThreshold :: Integer
                         , loss :: String
                         , printDistribution :: Bool
                         , uncensored :: Bool
                         , intervalFile :: FilePath
                         }

                deriving (Show, Data, Typeable) 

trainArgs = MTLRTrain { regConst1 = 1 &= explicit &= name "c" &= help "specify regularization constant C1 (default: 1)"
                      , regConst2 = 1 &= explicit &= name "d" &= help "specify regularization constant C2 (default: 1)"
                      , input = def &= explicit &= name "i" &= typDir &= help "input directory"
                      , output = "train-output" &= explicit &= name "o" &= typDir &= help "output directory"
                      , timePoints = 60 &= explicit &= name "m" &= help "specify the number of time points (default: 60)"
                      , weight = def &= explicit &= name "w" &= help "specify the weight (model) filename used to initialize EM training for censored targets"
                      , uncensored = def &= explicit &= name "u" &= help "treats all input examples during training as uncensored"
                      , intervalFile = def &= explicit &= name "q" &= help "interval file"
                      } &= help "Run MTLR training on a directory of data." &= explicit &= name "train"

testArgs = MTLRTest { input = def &= explicit &= name "i" &= typDir &= help "input directory"
                    , output = "test-output" &= explicit &= name "o" &= typDir &= help "output directory"
                    , timePoints = 60 &= explicit &= name "m" &= help "specify the number of time points (default: 60)"
                    , survivalThreshold = 30 &= explicit &= name "t" &= help "survival classification threashold"
                    , loss = "l1" &= explicit &= name "l" &= help "type of loss to optimize"
                    , printDistribution = def &= explicit &= name "p" &= help "print the survival distribution"
                    , uncensored = def &= explicit &= name "u" &= help "treats all input examples during training as uncensored"
                    , intervalFile = def &= explicit &= name "q" &= help "interval file"
                    } &= help "Run MTLR testing on a directory of data." &= explicit &= name "test"

main = do args <- cmdArgs (modes [trainArgs, testArgs] &= program "BatchMTLR")
          case args of
            MTLRTrain {input=""} -> error "Please specify an input directory!"
            MTLRTrain {} -> train args
            MTLRTest {input=""} -> error "Please specify an input directory!"
            MTLRTest {} -> undefined


-- | Run MTLR training.
train args = do dir <- readDirectoryWith return (input args)
                let filePairs =  map (createFilePair "model" (output args)) . toList $ dirTree dir
                handles <- runEverything  (proc "mtlr_train" mtlrArgs) {std_out = CreatePipe} filePairs
                mapM_ wait handles
  where wait (_,Just hout,_,processHandle) = do {output <- hGetContents hout; putStrLn output}
        mtlrArgs = [ "-c", show $ regConst1 args, "-d", show $ regConst2 args, "-m"
                   , show $ timePoints args, "-w", weight args
                   , "-u", if uncensored args then "1" else "0"
                   , "-q", intervalFile args
                   ]
                   
createFilePair :: String -> FilePath -> FilePath -> (FilePath, FilePath)
createFilePair outExt outDir inFile = (inFile, outFile)
  where outFile = combine outDir . joinPath . tail . splitPath $ replaceExtension inFile outExt

runEverything :: CreateProcess -> [(FilePath, FilePath)] -> IO [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)]
runEverything process = mapM (runFilePair process)

runFilePair :: CreateProcess -> (FilePath, FilePath) -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
runFilePair process (inFile, outFile) = do createDirectoryIfMissing True (takeDirectory outFile)
                                           createProcess newProcess
  where newProcess = process {cmdspec = newSpec}
        newSpec = case cmdspec process of
                      (ShellCommand str) -> ShellCommand (str ++ " -i " ++ inFile ++ " -o " ++ outFile)
                      (RawCommand cmd args) -> RawCommand cmd (args ++ ["-i", inFile, "-o", outFile])
