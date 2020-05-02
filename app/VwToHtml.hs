module VwToHtml where

import           System.Environment (getArgs)

import           Control.Monad      (when)

import           Lib                (debugArgs, isDebug)
import           LibVwToHtml        (vw2html)


main :: IO ()
main = do
    debug <- isDebug
    when debug debugArgs
    vw2html
