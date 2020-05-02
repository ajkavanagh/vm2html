module VwToHtml where

import System.Environment (getArgs)

import Control.Monad (when)

import LibVps (vw2html)
import Lib (debugArgs, isDebug)


main :: IO ()
main = do
    debug <- isDebug
    when debug debugArgs
    vw2html
