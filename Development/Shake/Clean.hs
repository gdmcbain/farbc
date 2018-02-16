module Development.Shake.Clean (cleanRules) where
    import Development.Shake
    import Development.Shake.FilePath

    clean :: [FilePattern] -> Rules ()
    clean patterns = "clean" ~> do
      need ["hlint"]   -- see cleanRules
      removeFilesAfter "."  $ "*~" : patterns

    hlint :: Rules ()
    hlint = "hlint" ~> cmd "hlint" "."

    cleanRules :: [FilePattern] -> Rules ()
    cleanRules patterns = do
      clean patterns
      hlint
