{-
   Select.hs (adapted from select.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This is an illustration of the selection mode and name stack, which detects
   whether objects which collide with a viewing volume. First, four triangles
   and a rectangular box representing a viewing volume are drawn (drawScene
   routine). The green triangle and yellow triangles appear to lie within the
   viewing volume, but the red triangle appears to lie outside it. Then the
   selection mode is entered (selectObjects routine). Drawing to the screen
   ceases. To see if any collisions occur, the four triangles are called. In
   this example, the green triangle causes one hit with the name 1, and the
   yellow triangles cause one hit with the name 3.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- draw a triangle with vertices at (x1, y1), (x2, y2) and (x3, y3) at z units
-- away from the origin.
drawTriangle ::
   Vertex2 GLfloat -> Vertex2 GLfloat -> Vertex2 GLfloat -> GLfloat -> IO ()
drawTriangle (Vertex2 x1 y1) (Vertex2 x2 y2) (Vertex2 x3 y3) z = do
   renderPrimitive Triangles $ mapM_ vertex [
      Vertex3 x1 y1 z,
      Vertex3 x2 y2 z,
      Vertex3 x3 y3 z]

-- draw a rectangular box with these outer x, y, and z values
drawViewVolume :: Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawViewVolume (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = do
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 1 1 1)
   renderPrimitive LineLoop $ mapM_ vertex [
      Vertex3 x1 y1 (-z1),
      Vertex3 x2 y1 (-z1),
      Vertex3 x2 y2 (-z1),
      Vertex3 x1 y2 (-z1)]

   renderPrimitive LineLoop $ mapM_ vertex [
      Vertex3 x1 y1 (-z2),
      Vertex3 x2 y1 (-z2),
      Vertex3 x2 y2 (-z2),
      Vertex3 x1 y2 (-z2)]

   renderPrimitive Lines $ mapM_ vertex [   -- 4 lines
      Vertex3 x1 y1 (-z1),
      Vertex3 x1 y1 (-z2),
      Vertex3 x1 y2 (-z1),
      Vertex3 x1 y2 (-z2),
      Vertex3 x2 y1 (-z1),
      Vertex3 x2 y1 (-z2),
      Vertex3 x2 y2 (-z1),
      Vertex3 x2 y2 (-z2)]

-- drawScene draws 4 triangles and a wire frame which represents the viewing
-- volume.
drawScene :: IO ()
drawScene = do
   matrixMode $= Projection
   loadIdentity
   perspective 40 (4/3) 1 100

   matrixMode $= Modelview 0
   loadIdentity
   lookAt (Vertex3 7.5 7.5 12.5) (Vertex3 2.5 2.5 (-5)) (Vector3 0 1 0)
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 0 1 0)   -- green triangle
   drawTriangle (Vertex2 2 2) (Vertex2 3 2) (Vertex2 2.5 3) (-5)
   color3f (Color3 1 0 0)   -- red triangle
   drawTriangle (Vertex2 2 7) (Vertex2 3 7) (Vertex2 2.5 8) (-5)
   color3f (Color3 1 1 0)   -- yellow triangles
   drawTriangle (Vertex2 2 2) (Vertex2 3 2) (Vertex2 2.5 3) (-1)
   drawTriangle (Vertex2 2 2) (Vertex2 3 2) (Vertex2 2.5 3) (-9)
   drawViewVolume (Vertex3 0 0 0) (Vertex3 5 5 10)

processHits :: Maybe [HitRecord] -> IO ()
processHits Nothing = putStrLn "selection buffer overflow"
processHits (Just hitRecords) = do
   putStrLn ("hits = " ++ show (length hitRecords))
   mapM_ (\(HitRecord z1 z2 names) -> do
      putStrLn (" number of names for hit = " ++ show (length names))
      putStr   ("  z1 is " ++ show z1)
      putStrLn ("; z2 is " ++ show z2)
      putStr   "   the name is"
      sequence_ [ putStr (" " ++ show n) | Name n <- names ]
      putChar '\n')
      hitRecords

-- selectObjects "draws" the triangles in selection mode, assigning names for
-- the triangles. Note that the third and fourth triangles share one name, so
-- that if either or both triangles intersects the viewing/clipping volume,
-- only one hit will be registered.

bufSize :: GLsizei
bufSize = 512

selectObjects :: IO ()
selectObjects = do
   (_, maybeHitRecords) <- getHitRecords bufSize $ do
      withName (Name 0) $ do
         preservingMatrix $ do
            matrixMode $= Projection
            loadIdentity
            ortho 0 5 0 5 0 10
            matrixMode $= Modelview 0
            loadIdentity
            loadName (Name 1)
            drawTriangle (Vertex2 2 2) (Vertex2 3 2) (Vertex2 2.5 3) (-5)
            loadName (Name 2)
            drawTriangle (Vertex2 2 7) (Vertex2 3 7) (Vertex2 2.5 8) (-5)
            loadName (Name 3)
            drawTriangle (Vertex2 2 2) (Vertex2 3 2) (Vertex2 2.5 3) (-1)
            drawTriangle (Vertex2 2 2) (Vertex2 3 2) (Vertex2 2.5 3) (-9)
      flush
   processHits maybeHitRecords

myInit :: IO ()
myInit = do
   depthFunc $= Just Less
   shadeModel $= Flat

display :: DisplayCallback
display = do
   clearColor $= Color4 0 0 0 0
   clear [ ColorBuffer, DepthBuffer ]
   drawScene
   selectObjects
   flush

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

-- Main Loop
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 200 200
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
