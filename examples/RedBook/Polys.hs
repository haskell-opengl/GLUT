{-
   Polys.hs (adapted from polys.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates polygon stippling.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

fly :: IO GLpolygonstipple
fly = newPolygonStipple [
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x03, 0x80, 0x01, 0xC0, 0x06, 0xC0, 0x03, 0x60,
   0x04, 0x60, 0x06, 0x20, 0x04, 0x30, 0x0C, 0x20,
   0x04, 0x18, 0x18, 0x20, 0x04, 0x0C, 0x30, 0x20,
   0x04, 0x06, 0x60, 0x20, 0x44, 0x03, 0xC0, 0x22,
   0x44, 0x01, 0x80, 0x22, 0x44, 0x01, 0x80, 0x22,
   0x44, 0x01, 0x80, 0x22, 0x44, 0x01, 0x80, 0x22,
   0x44, 0x01, 0x80, 0x22, 0x44, 0x01, 0x80, 0x22,
   0x66, 0x01, 0x80, 0x66, 0x33, 0x01, 0x80, 0xCC,
   0x19, 0x81, 0x81, 0x98, 0x0C, 0xC1, 0x83, 0x30,
   0x07, 0xe1, 0x87, 0xe0, 0x03, 0x3f, 0xfc, 0xc0,
   0x03, 0x31, 0x8c, 0xc0, 0x03, 0x33, 0xcc, 0xc0,
   0x06, 0x64, 0x26, 0x60, 0x0c, 0xcc, 0x33, 0x30,
   0x18, 0xcc, 0x33, 0x18, 0x10, 0xc4, 0x23, 0x08,
   0x10, 0x63, 0xC6, 0x08, 0x10, 0x30, 0x0c, 0x08,
   0x10, 0x18, 0x18, 0x08, 0x10, 0x00, 0x00, 0x08]

halftone :: IO GLpolygonstipple
halftone = newPolygonStipple . take 128 . cycle $ [
   0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55]

display :: (GLpolygonstipple, GLpolygonstipple) -> DisplayCallback
display (flyStipple, halftoneStipple) = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       rectf = rect :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()
   color3f (Color3 1 1 1)

   -- draw one solid, unstippled rectangle,
   -- then two stippled rectangles
   rectf (Vertex2  25 25) (Vertex2 125 125)
   polygonStipple $= Just flyStipple
   rectf (Vertex2 125 25) (Vertex2 225 125)
   polygonStipple $= Just halftoneStipple
   rectf (Vertex2 225 25) (Vertex2 325 125)
   polygonStipple $= (Nothing :: Maybe GLpolygonstipple)

   flush

myInit :: IO (GLpolygonstipple, GLpolygonstipple)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   flyStipple <- fly
   halftoneStipple <- halftone
   return (flyStipple, halftoneStipple)

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 350 150
   createWindow progName
   stipples <- myInit
   displayCallback $= display stipples
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
