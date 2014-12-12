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

import Data.List
import qualified Data.Foldable as F

import System.IO
import System.Console.CmdArgs
import System.Directory
import System.Directory.Tree hiding (name)
import System.FilePath
import System.Process

data MTLRArgs = MTLRTrain { regConst1 :: Double
                          , regConst2 :: Double
                          , input :: FilePath
                          , output :: FilePath
                          , timePoints :: Integer
                          , weight :: FilePath
                          , uncensored :: Integer
                          , intervalFile :: FilePath
                          }
                          
              | MTLRTest { input :: FilePath
                         , output :: FilePath
                         , timePoints :: Integer
                         , survivalThreshold :: Integer
                         , loss :: String
                         , printDistribution :: Bool
                         , uncensored :: Integer
                         , intervalFile :: FilePath
                         , printDir :: FilePath
                         }
                         
              | MTLRImputation { input :: FilePath
                               , method :: String
                               }

                deriving (Show, Data, Typeable) 

trainArgs = MTLRTrain { regConst1 = 1 &= explicit &= name "c" &= help "specify regularization constant C1 (default: 1)"
                      , regConst2 = 1 &= explicit &= name "d" &= help "specify regularization constant C2 (default: 1)"
                      , input = def &= explicit &= name "i" &= typDir &= help "input directory"
                      , output = def &= explicit &= name "o" &= typDir &= help "output directory"
                      , timePoints = 60 &= explicit &= name "m" &= help "specify the number of time points (default: 60)"
                      , weight = def &= explicit &= name "w" &= help "specify the weight (model) filename used to initialize EM training for censored targets"
                      , uncensored = 1 &= explicit &= name "u" &= help "treats all input examples during training as uncensored"
                      , intervalFile = def &= explicit &= name "q" &= help "interval file"
                      } &= help "Run MTLR training on a directory of data." &= explicit &= name "train"

testArgs = MTLRTest { input = def &= explicit &= name "i" &= typDir &= help "input directory"
                    , output = def &= explicit &= name "o" &= typDir &= help "model directory"
                    , timePoints = 60 &= explicit &= name "m" &= help "specify the number of time points (default: 60)"
                    , survivalThreshold = 30 &= explicit &= name "t" &= help "survival classification threashold"
                    , loss = "l1" &= explicit &= name "l" &= help "type of loss to optimize"
                    , printDistribution = False &= explicit &= name "p" &= help "print the survival distribution"
                    , uncensored = 1 &= explicit &= name "u" &= help "treats all input examples during training as uncensored"
                    , intervalFile = def &= explicit &= name "q" &= help "interval file"
                    , printDir = "test-output" &= explicit &= name "z" &= help "Where to print the output."
                    } &= help "Run MTLR testing on a directory of data." &= explicit &= name "test"
                    
modelArgs = MTLRImputation { input = def &= explicit &= name "i" &= typDir &= help "input directory"
                           , method = def &= explicit &= name "w" &= help "imputation method"
                           } &= help "Run imputation on all of the things." &= explicit &= name "imputation"

main = do args <- cmdArgs (modes [trainArgs, testArgs, modelArgs] &= program "BatchMTLR")
          case args of
            MTLRTrain {input=""} -> error "Please specify an input directory!"
            MTLRTrain {} -> train args
            MTLRTest {input=""} -> error "Please specify an input directory!"
            MTLRTest {} -> test args
            MTLRImputation {input=""} -> error "Please specify an input directory!"
            MTLRImputation {} -> imputation args


-- | Run MTLR training.
train args = do dir <- readDirectoryWith return (input args)
                let filePairs =  map (fileToArgs (output args)) . F.toList $ dirTree dir
                handles <- runEverything  (proc' "mtlr_train" mtlrArgs) {std_out = CreatePipe} filePairs
                mapM_ wait handles
  where wait (_,Just hout,_,processHandle) = do {output <- hGetContents hout; putStrLn output}
        mtlrArgs = [ "-c", show $ regConst1 args, "-d", show $ regConst2 args, "-m"
                   , show $ timePoints args, "-w", weight args
                   , "-u", show $ uncensored args
                   , if intervalFile args /= "" then "-q" else "", intervalFile args
                   ]

-- | Run MTLR testing.
test args = do dir <- readDirectoryWith return (input args)

               let filePairs =  map (fileToArgs (output args)) . F.toList $ dirTree dir
               let fileOutputs = map fileToPrint . F.toList $ dirTree dir

               handles <- runEverything  (proc' "mtlr_test" mtlrArgs) {std_out = CreatePipe} filePairs
               mapM_ wait (zip fileOutputs handles)

  where fileToPrint inFile = combine (printDir args) . joinPath . tail . splitPath $ replaceExtension inFile "out"
        wait (outFile, (_,Just hout,_,processHandle)) = do {createDirectoryIfMissing True (takeDirectory outFile); output <- hGetContents hout; writeFile outFile output}
        mtlrArgs = [ "-m", show $ timePoints args, "-l", loss args
                   , if printDistribution args then "-p" else ""
                   , "-u", show $ uncensored args
                   , if intervalFile args /= "" then "-q" else "", intervalFile args
                   ]
                   
-- | Run imputation stuffs.
imputation args = do dir <- readDirectoryWith return (input args)
                     let files = F.toList $ dirTree dir
                     mapM_ runImputation files
  where runImputation f = callProcess "Rscript" ["run_script.R", method args ++ ".imp", f, "10"]

fileToArgs :: FilePath -> FilePath -> [ArgumentFile]
fileToArgs modelDir inFile = [("-i", inFile), ("-o", modelFile)]
  where modelFile = combine modelDir . joinPath . tail . splitPath $ replaceExtension inFile "model"

proc' = proc

type ArgumentFile = (String, FilePath)

runEverything :: CreateProcess -> [[ArgumentFile]] -> IO [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)]
runEverything process = mapM (runFileArgs process)

runFileArgs :: CreateProcess -> [ArgumentFile] -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
runFileArgs process files = do mapM_ (createDirectoryIfMissing True) dirs
                               createProcess newProcess
  where newProcess = process {cmdspec = newSpec}
        dirs = map (takeDirectory . snd) files
        fileArgs = concatMap (\(arg, file) -> [arg, file]) files
        newSpec = case cmdspec process of
                      (ShellCommand str) -> ShellCommand (str ++ " " ++ (concat $ intersperse " " fileArgs))
                      (RawCommand cmd args) -> RawCommand cmd (args ++ fileArgs)
