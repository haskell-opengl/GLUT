{-
   TessWind.hs (adapted from tesswind.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates the winding rule polygon tessellation property.
   Four tessellated objects are drawn, each with very different contours. When
   the w key is pressed, the objects are drawn with a different winding rule.
-}

import Data.Char ( toLower )
import Data.Either ( Either(..) )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )
import Graphics.UI.GLUT

type DisplayLists = (DisplayList, DisplayList, DisplayList, DisplayList)

-- 'Float' is a dummy, any marshalable type would do
type DontCare = Float

rect1 :: ComplexContour DontCare
rect1 = ComplexContour [
   AnnotatedVertex (Vertex3  50  50 0) 0,
   AnnotatedVertex (Vertex3 300  50 0) 0,
   AnnotatedVertex (Vertex3 300 300 0) 0,
   AnnotatedVertex (Vertex3  50 300 0) 0 ]

rect2 :: ComplexContour DontCare
rect2 = ComplexContour [
   AnnotatedVertex (Vertex3 100 100 0) 0,
   AnnotatedVertex (Vertex3 250 100 0) 0,
   AnnotatedVertex (Vertex3 250 250 0) 0,
   AnnotatedVertex (Vertex3 100 250 0) 0 ]

rect3 :: ComplexContour DontCare
rect3 = ComplexContour [
   AnnotatedVertex (Vertex3 150 150 0) 0,
   AnnotatedVertex (Vertex3 200 150 0) 0,
   AnnotatedVertex (Vertex3 200 200 0) 0,
   AnnotatedVertex (Vertex3 150 200 0) 0 ]

rects1 :: ComplexPolygon DontCare
rects1 = ComplexPolygon [ rect1, rect2, rect3 ]

rects2 :: ComplexPolygon DontCare
rects2 = ComplexPolygon [
   rect1, reverseComplexContour rect2, reverseComplexContour rect3 ]

spiral :: ComplexPolygon DontCare
spiral = ComplexPolygon [
   ComplexContour [
      AnnotatedVertex (Vertex3 400 250 0) 0,
      AnnotatedVertex (Vertex3 400  50 0) 0,
      AnnotatedVertex (Vertex3  50  50 0) 0,
      AnnotatedVertex (Vertex3  50 400 0) 0,
      AnnotatedVertex (Vertex3 350 400 0) 0,
      AnnotatedVertex (Vertex3 350 100 0) 0,
      AnnotatedVertex (Vertex3 100 100 0) 0,
      AnnotatedVertex (Vertex3 100 350 0) 0,
      AnnotatedVertex (Vertex3 300 350 0) 0,
      AnnotatedVertex (Vertex3 300 150 0) 0,
      AnnotatedVertex (Vertex3 150 150 0) 0,
      AnnotatedVertex (Vertex3 150 300 0) 0,
      AnnotatedVertex (Vertex3 250 300 0) 0,
      AnnotatedVertex (Vertex3 250 200 0) 0,
      AnnotatedVertex (Vertex3 200 200 0) 0,
      AnnotatedVertex (Vertex3 200 250 0) 0 ] ]

quad1 :: ComplexContour DontCare
quad1 = ComplexContour [
   AnnotatedVertex (Vertex3  50 150 0) 0,
   AnnotatedVertex (Vertex3 350 150 0) 0,
   AnnotatedVertex (Vertex3 350 200 0) 0,
   AnnotatedVertex (Vertex3  50 200 0) 0 ]

quad2 :: ComplexContour DontCare
quad2 = ComplexContour [
   AnnotatedVertex (Vertex3 100 100 0) 0,
   AnnotatedVertex (Vertex3 300 100 0) 0,
   AnnotatedVertex (Vertex3 300 350 0) 0,
   AnnotatedVertex (Vertex3 100 350 0) 0 ]

tri :: ComplexContour DontCare
tri = ComplexContour [
   AnnotatedVertex (Vertex3 200  50 0) 0,
   AnnotatedVertex (Vertex3 250 300 0) 0,
   AnnotatedVertex (Vertex3 150 300 0) 0 ]

quadsAndTri :: ComplexPolygon DontCare
quadsAndTri = ComplexPolygon [ quad1, quad2, tri ]

reverseComplexContour :: ComplexContour DontCare -> ComplexContour DontCare
reverseComplexContour (ComplexContour avs) = ComplexContour (reverse avs)

makeNewLists :: IORef TessWinding -> DisplayLists -> IO ()
makeNewLists currentWindingRule (dl1, dl2, dl3, dl4) = do
   windingRule <- readIORef currentWindingRule
   print windingRule   -- not in original program, but useful
   compileList windingRule dl1 rects1
   compileList windingRule dl2 rects2
   compileList windingRule dl3 spiral
   compileList windingRule dl4 quadsAndTri

compileList :: TessWinding -> DisplayList -> ComplexPolygon DontCare -> IO ()
compileList windingRule displayList complexPolygon =
   defineList displayList Compile $ do
      simplePolygon <- checkForError $
         tessellate windingRule 0 (Normal3 0 0 0) noOpCombiner complexPolygon
      drawSimplePolygon simplePolygon

checkForError ::
   IO (Either Error (SimplePolygon DontCare)) -> IO (SimplePolygon DontCare)
checkForError action = do
   errorOrContours <- action
   case errorOrContours of
      Left (Error _category description) -> do
         putStrLn description
         exitFailure
      Right contours ->
         return contours

noOpCombiner :: Combiner DontCare
noOpCombiner _newVertex _weightedProperties = 0

drawSimplePolygon :: SimplePolygon DontCare -> IO ()
drawSimplePolygon (SimplePolygon primitives) =
   flip mapM_ primitives $ \(Primitive primitiveMode vertices) ->
      renderPrimitive primitiveMode $
         flip mapM_ vertices $ \(AnnotatedVertex plainVertex _) ->
            vertex plainVertex

display :: DisplayLists -> DisplayCallback
display (dl1, dl2, dl3, dl4) = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
   color3f (Color3 1 1 1)
   preservingMatrix $ do
      callList dl1
      translatef (Vector3   0   500  0)
      callList dl2
      translatef (Vector3 500 (-500) 0)
      callList dl3
      translatef (Vector3   0   500  0)
      callList dl4
   flush

myInit :: IORef TessWinding -> IO DisplayLists
myInit currentWindingRule = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   [dl1, dl2, dl3, dl4] <- genLists 4
   let displayLists = (dl1, dl2, dl3, dl4)
   makeNewLists currentWindingRule displayLists
   return displayLists

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D 0 1000 0 (1000 * hf/wf)
      else ortho2D 0 (1000 * wf/hf) 0 1000
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: IORef TessWinding -> DisplayLists -> KeyboardMouseCallback
keyboard currentWindingRule displayLists (Char c) Down _ _ = case toLower c of
   'w'   -> do modifyIORef currentWindingRule nextWindingRule
               makeNewLists currentWindingRule displayLists
               postRedisplay Nothing
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboard _ _ _ _ _ _ = return ()

nextWindingRule :: TessWinding -> TessWinding
nextWindingRule r = case r of 
   TessWindingOdd       -> TessWindingNonzero
   TessWindingNonzero   -> TessWindingPositive
   TessWindingPositive  -> TessWindingNegative
   TessWindingNegative  -> TessWindingAbsGeqTwo
   TessWindingAbsGeqTwo -> TessWindingOdd

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   createWindow progName
   currentWindingRule <- newIORef TessWindingOdd
   displayLists <- myInit currentWindingRule
   displayCallback $= display displayLists
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard currentWindingRule displayLists)
   mainLoop
