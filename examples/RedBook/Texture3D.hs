{-
   Texture3D.hs  (adapted from texture3d.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates using a three-dimensional texture. It creates
   a 3D texture and then renders two rectangles with different texture
   coordinates to obtain different "slices" of the 3D texture.
-}

import Control.Monad ( unless )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Create checkerboard image
imageSize :: TextureSize3D
imageSize = TextureSize3D 16 16 16

withImage :: (PixelData (Color3 GLubyte) -> IO ()) -> IO ()
withImage act =
   withArray [ Color3 (s * 17) (t * 17) (r * 17) |
               r <- [ 0 .. fromIntegral d - 1 ],
               t <- [ 0 .. fromIntegral h - 1 ],
               s <- [ 0 .. fromIntegral w - 1 ] ] $
   act . PixelData RGB UnsignedByte
   where (TextureSize3D w h d) = imageSize

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   [texName] <- genObjectNames 1
   textureBinding Texture3D $= Just texName
   textureWrapMode Texture3D S $= (Repeated, Clamp)
   textureWrapMode Texture3D T $= (Repeated, Clamp)
   textureWrapMode Texture3D R $= (Repeated, Clamp)
   textureFilter Texture3D $= ((Nearest, Nothing), Nearest)
   withImage $ texImage3D NoProxy 0  RGB' imageSize 0
   texture Texture3D $= Enabled

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   -- resolve overloading, not needed in "real" programs
   let texCoord3f = texCoord :: TexCoord3 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   renderPrimitive Quads $ do
      texCoord3f (TexCoord3 0 0 0); vertex3f (Vertex3 (-2.25) (-1) 0)
      texCoord3f (TexCoord3 0 1 0); vertex3f (Vertex3 (-2.25)   1  0)
      texCoord3f (TexCoord3 1 1 1); vertex3f (Vertex3 (-0.25)   1  0)
      texCoord3f (TexCoord3 1 0 1); vertex3f (Vertex3 (-0.25) (-1) 0)

      texCoord3f (TexCoord3 0 0 1); vertex3f (Vertex3   0.25  (-1) 0)
      texCoord3f (TexCoord3 0 1 1); vertex3f (Vertex3   0.25    1  0)
      texCoord3f (TexCoord3 1 1 0); vertex3f (Vertex3   2.25    1  0)
      texCoord3f (TexCoord3 1 0 0); vertex3f (Vertex3   2.25  (-1) 0)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 30
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-4 :: GLfloat))

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   exts <- get glExtensions
   unless ("GL_EXT_texture3D" `elem` exts) $ do
      putStrLn "Sorry, this demo requires the GL_EXT_texture3D extension."
      exitFailure
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
