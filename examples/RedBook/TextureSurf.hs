{-
   TextureSurf.hs (adapted from texturesurf.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program uses evaluators to generate a curved surface and automatically
   generated texture coordinates.
-}

import Data.List ( transpose )
import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

ctrlPoints :: [[Vertex3 GLfloat]]
ctrlPoints = [
   [ Vertex3 (-1.5) (-1.5)   4.0,  Vertex3 (-0.5) (-1.5)   2.0,
     Vertex3   0.5  (-1.5) (-1.0), Vertex3   1.5  (-1.5)   2.0 ],
   [ Vertex3 (-1.5) (-0.5)   1.0,  Vertex3 (-0.5) (-0.5)   3.0,
     Vertex3   0.5  (-0.5)   0.0,  Vertex3   1.5  (-0.5) (-1.0) ],
   [ Vertex3 (-1.5)   0.5    4.0,  Vertex3 (-0.5)   0.5    0.0,
     Vertex3   0.5    0.5    3.0,  Vertex3   1.5    0.5    4.0 ],
   [ Vertex3 (-1.5)   1.5  (-2.0), Vertex3 (-0.5)   1.5  (-2.0),
     Vertex3   0.5    1.5    0.0,  Vertex3   1.5    1.5  (-1.0) ]]

texPts :: [[TexCoord2 GLfloat]]
texPts = [
   [ TexCoord2 0 0, TexCoord2 0 1 ],
   [ TexCoord2 1 0, TexCoord2 1 1 ]]

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   color (Color3 1 1 1 :: Color3 GLfloat)
   evalMesh2 Fill (0, 20) (0, 20)
   flush

imageSize :: TextureSize2D
imageSize = TextureSize2D 64 64

withImage :: (PixelData (Color3 GLubyte) -> IO ()) -> IO ()
withImage act =
   withArray [ Color3 (s (sin ti)) (s (cos (2 * tj))) (s (cos (ti + tj))) |
               i <- [ 0 .. fromIntegral w - 1 ],
               let ti = 2 * pi * i / fromIntegral w,
               j <- [ 0 .. fromIntegral h - 1 ],
               let tj = 2 * pi * j / fromIntegral h ] $
   act . PixelData RGB UnsignedByte
   where (TextureSize2D w h) = imageSize
         s :: Double -> GLubyte
         s x = truncate (127 * (1 + x))

myInit :: IO ()
myInit = do
   m <- newMap2 (0, 1) (0, 1) (transpose ctrlPoints)
   map2 $= Just (m :: GLmap2 Vertex3 GLfloat)
   t <- newMap2 (0, 1) (0, 1) (transpose texPts)
   map2 $= Just (t :: GLmap2 TexCoord2 GLfloat)
   mapGrid2 $= ((20, (0, 1)), (20, (0, 1 :: GLfloat)))
   textureFunction $= Decal
   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withImage $ texImage2D Nothing NoProxy 0 RGB' imageSize 0
   texture Texture2D $= Enabled
   depthFunc $= Just Less
   shadeModel $= Flat

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
   matrixMode $= Modelview 0
   loadIdentity
   rotate (85 :: GLfloat) (Vector3 1 1 1)

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
