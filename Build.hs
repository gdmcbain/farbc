#!/usr/bin/env stack
{-stack runhaskell
 --package shake
 -}

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Clean (cleanRules)

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["farbc.pdf"]

  cleanRules ["*.blg", "*.aux", "*.dvi", "*.log", "*.pdf",
               "octave-workspace"]

  "*.pdf" %> \pdf -> do
    let tex = pdf -<.> "tex"
    need [tex]
    cmd "pdflatex" tex
