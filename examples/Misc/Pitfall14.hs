{-
   Pitfall14
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates pitfall 14 (Careful Enabling Color Material)
   of Mark Kilgard's "16 Common OpenGL Pitfalls", see:
   http://www.opengl.org/resources/features/KilgardTechniques/oglpitfall/
-}

import Control.Monad ( unless )
import System.Exit ( exitFailure )
import Graphics.UI.GLUT

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   createWindow progName

   currentColor $= Color4 1 1 1 1
   materialAmbient Front $= Color4 0.1 0.1 0.1 1
   -- re-get to avoid any rounding issues
   mafBefore <- get (materialAmbient Front)

   colorMaterial $= Just (Front, Diffuse)
   mafAfter <- get (materialAmbient Front)
   unless (mafBefore == mafAfter) $ do
      putStrLn "ERROR: The ambient material property changed!"
      putStrLn ("   before: " ++ show mafBefore)
      putStrLn ("   after : " ++ show mafAfter)
      exitFailure
