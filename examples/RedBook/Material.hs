{-
   Material.hs (adapted from material.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates the use of the GL lighting model. Several
   objects are drawn using different material characteristics. A single
   light source illuminates the objects.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Initialize z-buffer, projection matrix, light source, and lighting model.
-- Do not specify a material property here.

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0.1 0.1 0
   depthFunc $= Just Less
   shadeModel $= Smooth

   ambient (Light 0) $= Color4 0 0 0 1
   diffuse (Light 0) $= Color4 1 1 1 1
   position (Light 0) $= Vertex4 0 3 2 0
   lightModelAmbient $= Color4 0.4 0.4 0.4 1
   lightModelLocalViewer $= Disabled

   lighting $= Enabled
   light (Light 0) $= Enabled

-- Draw twelve spheres in 3 rows with 4 columns.
-- The spheres in the first row have materials with no ambient reflection.
-- The second row has materials with significant ambient reflection.
-- The third row has materials with colored ambient reflection.
--
-- The first column has materials with blue, diffuse reflection only.
-- The second column has blue diffuse reflection, as well as specular
-- reflection with a low shininess exponent.
-- The third column has blue diffuse reflection, as well as specular
-- reflection with a high shininess exponent (a more concentrated highlight).
-- The fourth column has materials which also include an emissive component.
--
-- translate is used to move spheres to their appropriate locations.

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]

   let draw :: GLfloat -> GLfloat -> Color4 GLfloat -> Color4 GLfloat -> Color4 GLfloat -> GLfloat -> Color4 GLfloat -> IO ()
       draw row column amb dif spc shi emi =
          preservingMatrix $ do
             translate (Vector3 (2.5 * (column - 2.5)) (3 * (2 - row)) 0)
             materialAmbient   Front $= amb
             materialDiffuse   Front $= dif
             materialSpecular  Front $= spc
             materialShininess Front $= shi
             materialEmission  Front $= emi
             renderObject Solid (Sphere' 1 16 16)

       noMat           = Color4 0 0 0 1
       matAmbient      = Color4 0.7 0.7 0.7 1
       matAmbientColor = Color4 0.8 0.8 0.2 1
       matDiffuse      = Color4 0.1 0.5 0.8 1
       matSpecular     = Color4 1 1 1 1
       noShininess     = 0
       lowShininess    = 5
       highShininess   = 100
       matEmission     = Color4 0.3 0.2 0.2 0

   -- draw sphere in first row, first column
   -- diffuse reflection only; no ambient or specular
   draw 1 1 noMat matDiffuse noMat noShininess noMat

   -- draw sphere in first row, second column
   -- diffuse and specular reflection; low shininess; no ambient
   draw 1 2 noMat matDiffuse matSpecular lowShininess noMat

   -- draw sphere in first row, third column
   -- diffuse and specular reflection; high shininess; no ambient
   draw 1 3 noMat matDiffuse matSpecular highShininess noMat

   -- draw sphere in first row, fourth column
   -- diffuse reflection; emission; no ambient or specular reflection
   draw 1 4 noMat matDiffuse noMat noShininess matEmission

   -- draw sphere in second row, first column
   -- ambient and diffuse reflection; no specular
   draw 2 1 matAmbient matDiffuse noMat noShininess noMat

   -- draw sphere in second row, second column
   -- ambient, diffuse and specular reflection; low shininess
   draw 2 2 matAmbient matDiffuse matSpecular lowShininess noMat

   -- draw sphere in second row, third column
   -- ambient, diffuse and specular reflection; high shininess
   draw 2 3 matAmbient matDiffuse matSpecular highShininess noMat

   -- draw sphere in second row, fourth column
   -- ambient and diffuse reflection; emission; no specular
   draw 2 4 matAmbient matDiffuse noMat noShininess matEmission

   -- draw sphere in third row, first column
   -- colored ambient and diffuse reflection; no specular
   draw 3 1 matAmbientColor matDiffuse noMat noShininess noMat

   -- draw sphere in third row, second column
   -- colored ambient, diffuse and specular reflection; low shininess
   draw 3 2 matAmbientColor matDiffuse matSpecular lowShininess noMat

   -- draw sphere in third row, third column
   -- colored ambient, diffuse and specular reflection; high shininess
   draw 3 3 matAmbientColor matDiffuse matSpecular highShininess noMat

   -- draw sphere in third row, fourth column
   -- colored ambient and diffuse reflection; emission; no specular
   draw 3 4 matAmbientColor matDiffuse noMat noShininess matEmission

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h * 2
      then ortho (-6) 6 (-3 * (hf * 2) / wf) (3 * (hf * 2) / wf) (-10) 10
      else ortho (-6 * wf / (hf * 2)) (6 * wf / (hf * 2)) (-3) 3 (-10) 10
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 600 450
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
