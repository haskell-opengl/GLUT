{-
   Stroke.hs (adapted from stroke.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates some characters of a stroke (vector) font. The
   characters are represented by display lists, which are given numbers which
   correspond to the ASCII values of the characters. Use of callLists is
   demonstrated.
-}

import Data.List ( genericLength )
import Foreign.C.String ( castCharToCChar )
import Foreign.Marshal.Array ( withArray )
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

aData, eData, pData, rData, sData :: [[Vertex2 GLfloat]]
aData = [
   [ Vertex2 0 0, Vertex2 0 9, Vertex2 1 10, Vertex2 4 10, Vertex2 5 9,
     Vertex2 5 0 ],
   [ Vertex2 0 5, Vertex2 5 5 ] ]

eData = [
   [ Vertex2 5 0, Vertex2 0 0, Vertex2 0 10, Vertex2 5 10 ],
   [ Vertex2 0 5, Vertex2 4 5 ] ]

pData = [
   [ Vertex2 0 0, Vertex2 0 10,  Vertex2 4 10, Vertex2 5 9, Vertex2 5 6,
     Vertex2 4 5, Vertex2 0 5 ] ]

rData = [
   [ Vertex2 0 0, Vertex2 0 10,  Vertex2 4 10, Vertex2 5 9, Vertex2 5 6,
     Vertex2 4 5, Vertex2 0 5 ],
   [ Vertex2 3 5, Vertex2 5 0 ] ]

sData = [
   [ Vertex2 0 1, Vertex2 1 0, Vertex2 4 0, Vertex2 5 1, Vertex2 5 4,
     Vertex2 4 5, Vertex2 1 5, Vertex2 0 6, Vertex2 0 9, Vertex2 1 10,
     Vertex2 4 10, Vertex2 5 9 ] ]

advance :: IO ()
advance = translate (Vector3 8 0 (0 :: GLfloat))

-- drawLetter renders a letter with line segments given by the list of line
-- strips.
drawLetter :: [[Vertex2 GLfloat]] -> IO ()
drawLetter lineStrips = do
   mapM_ (renderPrimitive LineStrip . mapM_ vertex) lineStrips
   advance

charToGLubyte :: Char -> GLubyte
charToGLubyte = fromIntegral . castCharToCChar

myInit :: IO ()
myInit = do
   shadeModel $= Flat

   (base@(DisplayList b):_) <- genObjectNames 128
   listBase $= base
   let charToDisplayList c = DisplayList (b + fromIntegral (charToGLubyte c))
   mapM_ (\(c, d) -> defineList (charToDisplayList c) Compile d)
         [ ('A', drawLetter aData),
           ('E', drawLetter eData),
           ('P', drawLetter pData),
           ('R', drawLetter rData),
           ('S', drawLetter sData),
           (' ', advance) ]

test1, test2 :: String
test1 = "A SPARE SERAPE APPEARS AS"
test2 = "APES PREPARE RARE PEPPERS"

printStrokedString :: String -> IO ()
printStrokedString s =
   withArray (map charToGLubyte s) $
      callLists (genericLength s) UnsignedByte

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]

   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()

   color3f (Color3 1 1 1)
   preservingMatrix $ do
      scalef 2 2 2
      translatef (Vector3 10 30 0)
      printStrokedString test1
   preservingMatrix $ do
      scalef 2 2 2
      translatef (Vector3 10 13 0)
      printStrokedString test2
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char c) Down _ _ = case c of
   ' '   -> postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 440 120
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display
   mainLoop
