{-
   PickDepth.hs (adapted from pickdepth.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Picking is demonstrated in this program. In rendering mode, three
   overlapping rectangles are drawn. When the left mouse button is pressed,
   selection mode is entered with the picking matrix. Rectangles which are drawn
   under the cursor position are "picked." Pay special attention to the depth
   value range, which is returned.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   depthFunc $= Just Less
   shadeModel $= Flat
   depthRange $= (0, 1)   -- The default z mapping

-- The nine squares are drawn. Each square is given two names: one for the row
-- and the other for the column on the grid. The color of each square is
-- determined by its position on the grid, and the value in the board array.
-- Note: In contrast to the the original example, we always give names to
-- squares, regardless of the render mode. This simplifies the code a bit and
-- is even suggested by the Red Book.

-- The three rectangles are drawn, each with a different name. Note that each
-- rectangle is drawn with a different z value. Note: In contrast to the the
-- original example, we always give names to squares, regardless of the render
-- mode. This simplifies the code a bit and is even suggested by the Red Book.
drawRects :: IO ()
drawRects = do
   -- resolve overloading, not needed in "real" programs
   let color3  = color :: Color3 GLfloat -> IO ()
       vertex3 = vertex :: Vertex3 GLint -> IO ()
   loadName (Name 1)
   renderPrimitive Quads $ do
      color3 (Color3 1.0 1.0 0.0)
      vertex3 (Vertex3 2 0 0)
      vertex3 (Vertex3 2 6 0)
      vertex3 (Vertex3 6 6 0)
      vertex3 (Vertex3 6 0 0)
   loadName (Name 2)
   renderPrimitive Quads $ do
      color3 (Color3 0.0 1.0 1.0)
      vertex3 (Vertex3 3 2 (-1))
      vertex3 (Vertex3 3 8 (-1))
      vertex3 (Vertex3 8 8 (-1))
      vertex3 (Vertex3 8 2 (-1))
   loadName (Name 3)
   renderPrimitive Quads $ do
      color3 (Color3 1.0 0.0 1.0)
      vertex3 (Vertex3 0 2 (-2))
      vertex3 (Vertex3 0 7 (-2))
      vertex3 (Vertex3 5 7 (-2))
      vertex3 (Vertex3 5 2 (-2))

-- processHits prints the hit records.
processHits :: Maybe[HitRecord] -> IO ()
processHits Nothing = putStrLn "selection buffer overflow"
processHits (Just hitRecords) = do
   putStrLn ("hits = " ++ show (length hitRecords))
   flip mapM_ hitRecords $ \(HitRecord z1 z2 names) -> do
      putStrLn (" number of names for hit = " ++ show (length names))
      putStr   ("  z1 is " ++ show z1)
      putStrLn ("; z2 is " ++ show z2)
      putStr   "   the name is"
      sequence_ [ putStr (" " ++ show n) | Name n <- names ]
      putChar '\n'

-- pickRects() sets up selection mode, name stack, and projection matrix for
-- picking. Then the objects are drawn.

bufSize :: GLsizei
bufSize = 512

pickRects :: KeyboardMouseCallback
pickRects (MouseButton LeftButton) Down _ (Position x y) = do
   vp@(_, (Size _ height)) <- get viewport
   (_, maybeHitRecords) <- getHitRecords bufSize $
      withName (Name 0) $ do
         matrixMode $= Projection
         preservingMatrix $ do
            loadIdentity
            -- create 5x5 pixel picking region near cursor location
            pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (5, 5) vp
            ortho 0 8 0 8 (-0.5) 2.5
            drawRects
         flush
   processHits maybeHitRecords
   postRedisplay Nothing
pickRects (Char '\27') Down _ _ = exitWith ExitSuccess
pickRects _            _    _ _ = return ()

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   drawRects
   flush

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho 0 8 0 8 (-0.5) 2.5
   matrixMode $= Modelview 0
   loadIdentity

-- Main Loop
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 200 200
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just pickRects
   mainLoop
