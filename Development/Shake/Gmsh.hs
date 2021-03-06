module Development.Shake.Gmsh (gmshRules, Dimension(..)) where
    import Development.Shake
    import Development.Shake.FilePath

{- To use this, just import Development.Shake.Gmsh and put, e.g.,
 -
 -     gmshRules TwoD
 -
 - under the "do" in the main in the Build.hs script.  gmshRules needs the
 - Dimension.
 -}

    data Dimension = ZeroD | OneD | TwoD | ThreeD deriving Enum

    instance Show Dimension where
      show = ('-' :) . show . fromEnum

    msh :: Dimension -> Rules ()
    msh d = "*.msh" %> \msh -> do
      let geo = msh -<.> "geo"
      need [geo]
      gmshFlags <- getEnv "GMSHFLAGS"
      cmd "gmsh" (show d) gmshFlags [geo]

    medit :: Dimension -> Rules ()
    medit d = "*.medit" %> \out -> do
      let geo = out -<.> "geo"
      need [geo]
      gmshFlags <- getEnv "GMSHFLAGS"
      cmd "gmsh" (show d) gmshFlags
        "-format" "mesh" "-string" "Mesh.SaveElementTagType=2;"
        "-o" [out] [geo] :: Action ()
      removeFilesAfter "." [out]

    mesh :: Dimension -> Rules ()

    mesh ThreeD = "*.mesh" %> \mesh -> do
      let medit = mesh -<.> "medit"
      need [medit]
      copyFileChanged mesh medit

    mesh TwoD = "*.mesh" %> \mesh -> do
      let medit = mesh -<.> "medit"
      home <- getEnvWithDefault "../.." "HOME"
      let ffScripts = home </> "src/freefem/scripts"
          script = "3to2.awk"   -- gitlab:msm/freefem/scripts
      need [medit, ffScripts </> script]
      Stdout stdout <- cmd (AddEnv "AWKPATH" ffScripts)
                           "gawk" "-f" script [medit]
      writeFileChanged mesh stdout

    gmshRules :: Dimension -> Rules()
    gmshRules d = do
      msh d
      medit d
      mesh d
