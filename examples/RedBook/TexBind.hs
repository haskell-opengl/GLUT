{-
   TexBind.hs  (adapted from texbind.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates using textureBinding by creating and managing
   two textures.
-}

import Control.Monad ( when )
import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Create checkerboard image
checkImageSize :: TextureSize2D
checkImageSize = TextureSize2D 64 64

withCheckImage :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withCheckImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ] $
   act . PixelData RGBA UnsignedByte

myInit :: IO (TextureObject, TextureObject)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   [texName0, texName1] <- genObjectNames 2
   textureBinding Texture2D $= Just texName0
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withCheckImage checkImageSize 0x08 (\c -> Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0

   textureBinding Texture2D $= Just texName1
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   textureFunction $= Decal
   withCheckImage checkImageSize 0x10 (\c -> Color4 c 0 0 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0
   texture Texture2D $= Enabled
   return (texName0, texName1)

display ::  (TextureObject, TextureObject) -> DisplayCallback
display (texName0, texName1) = do
   clear [ ColorBuffer, DepthBuffer ]
   -- resolve overloading, not needed in "real" programs
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   textureBinding Texture2D $= Just texName0
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-2.0)    (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-2.0)      1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   0.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   0.0     (-1.0)   0.0     )
   textureBinding Texture2D $= Just texName1
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   2.41421   1.0  (-1.41421))
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   2.41421 (-1.0) (-1.41421))
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 30
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-3.6 :: GLfloat))

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
   version <- get glVersion
   when (take 3 version == "1.0") $ do
      putStrLn "This program demonstrates a feature which is not in OpenGL Version 1.0."
      putStrLn "If your implementation of OpenGL Version 1.0 has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure
   texNames <- myInit
   reshapeCallback $= Just reshape
   displayCallback $= display texNames
   keyboardMouseCallback $= Just keyboard
   mainLoop
