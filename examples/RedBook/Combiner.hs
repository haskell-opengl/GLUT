{-
   Combiner.hs  (adapted from combiner.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program renders a variety of quads showing different effects of
   texture combiner functions.

   The first row renders an untextured polygon (so you can compare the
   fragment colors) and then the 2 textures.

   The second row shows several different combiner functions on a single
   texture: replace, modulate, add, add-signed, and subtract.

   The third row shows the interpolate combiner function on a single texture
   with a constant color/alpha value, varying the amount of interpolation.

   The fourth row uses multitexturing with two textures and different
   combiner functions.

   The fifth row are some combiner experiments: using the scaling factor and
   reversing the order of subtraction for a combination function.
-}

import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Create checkerboard image
imageSize :: TextureSize2D
imageSize = TextureSize2D 8 8

makeImage :: TextureSize2D -> (GLsizei -> GLsizei -> Color4 GLubyte)
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
makeImage (TextureSize2D w h) f act =
   withArray [ f i j |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ] ] $
   act . PixelData RGBA UnsignedByte

myInit :: IO (TextureObject, TextureObject, DisplayList)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth

   rowAlignment Unpack $= 1

   [texName0, texName1] <- genObjectNames 2

   textureBinding Texture2D $= Just texName0
   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   -- horiz b & w stripes
   makeImage imageSize (\i _ -> let c = if i .&. 2 == 0 then 255 else 0 in Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' imageSize 0

   textureBinding Texture2D $= Just texName1
   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   textureFunction $= Decal
   -- wider vertical 50% cyan and black stripes
   makeImage imageSize (\_ j -> let c = if j .&. 4 /= 0 then 128 else 0 in Color4 0 c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' imageSize 0

   -- smooth-shaded polygon with multiple texture coordinates
   let vert :: TexCoord2 GLfloat -> Color3 GLfloat -> Vertex3 GLfloat -> IO ()
       vert t c v = do
          multiTexCoord (TextureUnit 0) t
          multiTexCoord (TextureUnit 1) t
          color c
          vertex v

   dl <- defineNewList Compile $
      renderPrimitive Quads $ do
         vert (TexCoord2 0 0) (Color3 0.5 1   0.25) (Vertex3 0 0 0)
         vert (TexCoord2 0 2) (Color3 1   1   1   ) (Vertex3 0 1 0)
         vert (TexCoord2 2 2) (Color3 1   1   1   ) (Vertex3 1 1 0)
         vert (TexCoord2 2 0) (Color3 1   0.5 0.25) (Vertex3 1 0 0)

   return (texName0, texName1, dl)

display ::  (TextureObject, TextureObject, DisplayList) -> DisplayCallback
display (texName0, texName1, dl) = do
   clear [ ColorBuffer ]

   let drawAt :: GLfloat -> GLfloat -> IO ()
       drawAt x y = preservingMatrix $ do
                   translate (Vector3 x y 0)
                   callList dl

   -- untextured polygon -- see the "fragment" colors
   texture Texture2D $= Disabled
   drawAt 0 5

   texture Texture2D $= Enabled
   -- draw ordinary textured polys; 1 texture unit; combine mode disabled
   textureFunction $= Modulate
   textureBinding Texture2D $= Just texName0
   drawAt 1 5

   textureBinding Texture2D $= Just texName1
   drawAt 2 5

   -- different combine modes enabled; 1 texture unit
   -- defaults are:
   -- argRGB Arg0 $= Arg SrcColor CurrentUnit
   -- argRGB Arg1 $= Arg SrcColor Previous

   textureBinding Texture2D $= Just texName0
   textureFunction $= Combine
   combineRGB $= Replace'
   argRGB Arg0 $= Arg SrcColor CurrentUnit
   drawAt 1 4

   combineRGB $= Modulate'
   argRGB Arg1 $= Arg SrcColor Previous
   drawAt 2 4

   combineRGB $= AddUnsigned'
   drawAt 3 4

   combineRGB $= AddSigned
   drawAt 4 4

   combineRGB $= Subtract
   drawAt 5 4

   -- interpolate combine with constant color; 1 texture unit 
   -- use different alpha values for constant color
   -- defaults are:
   -- argRGB Arg0 $= Arg SrcColor CurrentUnit
   -- argRGB Arg1 $= Arg SrcColor Previous
   -- argRGB Arg2 $= Arg SrcAlpha Constant

   constantColor $= Color4 0 0 0 0.2
   textureBinding Texture2D $= Just texName0
   textureFunction $= Combine
   combineRGB $= Interpolate
   argRGB Arg0 $= Arg SrcColor CurrentUnit
   argRGB Arg1 $= Arg SrcColor Previous
   argRGB Arg2 $= Arg SrcAlpha Constant
   drawAt 1 3

   constantColor $= Color4 0 0 0 0.4
   drawAt 2 3

   constantColor $= Color4 0 0 0 0.6
   drawAt 3 3

   constantColor $= Color4 0 0 0 0.8
   drawAt 4 3

   -- combine textures 0 & 1
   -- defaults are:
   -- argRGB Arg0 $= Arg SrcColor CurrentUnit
   -- argRGB Arg1 $= Arg SrcColor Previous

   activeTexture $= TextureUnit 0
   texture Texture2D $= Enabled
   textureBinding Texture2D $= Just texName0
   textureFunction $= Modulate

   activeTexture $= TextureUnit 1
   texture Texture2D $= Enabled
   textureBinding Texture2D $= Just texName1
   textureFunction $= Combine
   combineRGB $= Replace'
   drawAt 1 2

   -- try different combiner modes of texture unit 1
   combineRGB $= Modulate'
   drawAt 2 2

   combineRGB $= AddUnsigned'
   drawAt 3 2

   combineRGB $= AddSigned
   drawAt 4 2

   combineRGB $= Subtract
   drawAt 5 2

   -- some experiments

   -- see the effect of rgbScale
   rgbScale $= 2
   combineRGB $= Replace'
   drawAt 1 1

   combineRGB $= Modulate'
   drawAt 2 1
   rgbScale $= 1

   -- reverse the order of subtraction Arg1-Arg0

   textureFunction $= Combine
   combineRGB $= Subtract
   argRGB Arg0 $= Arg SrcColor Previous
   argRGB Arg1 $= Arg SrcColor CurrentUnit
   drawAt 5 1

   activeTexture $= TextureUnit 1   -- deactivate multitexturing
   texture Texture2D $= Disabled
   activeTexture $= TextureUnit 0   -- activate single texture unit

   flush

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 7 0 7
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 400 400
   initialWindowPosition $= Position 100 100
   createWindow progName
   texNamesAndDL <- myInit
   displayCallback $= display texNamesAndDL
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
