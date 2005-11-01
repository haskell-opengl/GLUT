{-
   Checker.hs  (adapted from checker.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program texture maps a checkerboard image onto two rectangles.

   Texture objects are only used when GL_EXT_texture_object is supported.
-}

import Control.Monad ( when )
import Data.Maybe ( isJust, listToMaybe )
import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
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
   act. PixelData RGBA UnsignedByte

myInit :: IO (Maybe TextureObject)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   exts <- get glExtensions
   mbTexName <- if "GL_EXT_texture_object" `elem` exts
                   then fmap listToMaybe $ genObjectNames 1
                   else return Nothing
   when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName

   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withCheckImage checkImageSize 0x8 (\c -> Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0
   return mbTexName

display ::  Maybe TextureObject -> DisplayCallback
display mbTexName = do
   clear [ ColorBuffer, DepthBuffer ]
   texture Texture2D $= Enabled
   textureFunction $= Decal
   when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName
   
   -- resolve overloading, not needed in "real" programs
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-2.0)    (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-2.0)      1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   0.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   0.0     (-1.0)   0.0     )

      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   2.41421   1.0  (-1.41421))
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   2.41421 (-1.0) (-1.41421))
   flush
   texture Texture2D $= Disabled

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
   mbTexName <- myInit
   displayCallback $= display mbTexName
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
