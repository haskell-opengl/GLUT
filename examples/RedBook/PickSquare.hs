{-
   PickSquare.hs (adapted from picksquare.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Use of multiple names and picking are demonstrated. A 3x3 grid of squares is
   drawn. When the left mouse button is pressed, all squares under the cursor
   position have their color changed.
-}

import Data.Array ( Array, listArray, (!) )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

type Board = Array (GLint,GLint) (IORef Int)

data State = State { board :: Board }

makeState :: IO State
makeState = do
   refs <- sequence . replicate 9 . newIORef $ 0
   return $ State { board = listArray ((0,0),(2,2)) refs }

-- Clear color value for every square on the board
myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0

-- The nine squares are drawn. Each square is given two names: one for the row
-- and the other for the column on the grid. The color of each square is
-- determined by its position on the grid, and the value in the board array.
-- Note: In contrast to the the original example, we always give names to
-- squares, regardless of the render mode. This simplifies the code a bit and
-- is even suggested by the Red Book.
drawSquares :: State -> IO ()
drawSquares state =
   flip mapM_ [ 0 .. 2 ] $ \i -> do
      loadName (Name (fromIntegral i))
      flip mapM_ [ 0 .. 2 ] $ \j ->
         withName (Name (fromIntegral j)) $ do
            val <- get (board state ! (i,j))
            -- resolve overloading, not needed in "real" programs
            let color3f = color :: Color3 GLfloat -> IO ()
            color3f (Color3 (fromIntegral i   / 3.0)
                            (fromIntegral j   / 3.0)
                            (fromIntegral val / 3.0))
            rect (Vertex2 i j) (Vertex2 (i + 1) (j + 1))

-- processHits prints the hit records and updates the board array.
processHits :: Maybe[HitRecord] -> State -> IO ()
processHits Nothing _ = putStrLn "selection buffer overflow"
processHits (Just hitRecords) state = do
   putStrLn ("hits = " ++ show (length hitRecords))
   mapM_ (\(HitRecord z1 z2 names) -> do
      putStrLn (" number of names for this hit = " ++ show (length names))
      putStr   ("  z1 is " ++ show z1)
      putStrLn ("; z2 is " ++ show z2)
      putStr   "   names are"
      sequence_ [ putStr (" " ++ show n) | Name n <- names ]
      putChar '\n'
      let [i, j] = [ fromIntegral n | Name n <- names ]
      (board state ! (i,j)) $~ (\x -> (x + 1) `mod` 3))
      hitRecords

-- pickSquares sets up selection mode, name stack, and projection matrix for
-- picking. Then the objects are drawn.

bufSize :: GLsizei
bufSize = 512

pickSquares :: State -> KeyboardMouseCallback
pickSquares state (MouseButton LeftButton) Down _ (Position x y) = do
   vp@(_, (Size _ height)) <- get viewport
   (_, maybeHitRecords) <- getHitRecords bufSize $
      withName (Name 0) $ do
         matrixMode $= Projection
         preservingMatrix $ do
            loadIdentity
            -- create 5x5 pixel picking region near cursor location
            pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (5, 5) vp
            ortho2D 0 3 0 3
            drawSquares state
         flush
   processHits maybeHitRecords state
   postRedisplay Nothing
pickSquares _ (Char '\27') Down _ _ = exitWith ExitSuccess
pickSquares _ _            _    _ _ = return ()

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   drawSquares state
   flush

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 3 0 3
   matrixMode $= Modelview 0
   loadIdentity

-- Main Loop
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 100 100
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display state
   keyboardMouseCallback $= Just (pickSquares state)
   mainLoop
