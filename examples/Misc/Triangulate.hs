{-
   Triangulate.hs (adapted from tess.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates polygon triangulation. Two tesselated objects are
   drawn. The first is a rectangle with a triangular hole. The second is a
   smooth shaded, self-intersecting star.

   Note the exterior rectangle is drawn with its vertices in counter-clockwise
   order, but its interior clockwise. Note the combineCallback is needed for the
   self-intersecting star. Also note that removing the TessProperty for the
   star will make the interior unshaded (TessWindingOdd).
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.Random ( randomIO )
import Graphics.UI.GLUT

display :: [DisplayList] -> DisplayCallback
display displayLists = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 1 1 1)
   mapM callList displayLists
   flush

-- 'Float' is a dummy, any marshalable type would do
type DontCare = Float

rectangle :: ComplexContour DontCare
rectangle = ComplexContour [
   AnnotatedVertex (Vertex3  50  50 0) 0,
   AnnotatedVertex (Vertex3 200  50 0) 0,
   AnnotatedVertex (Vertex3 200 200 0) 0,
   AnnotatedVertex (Vertex3  50 200 0) 0 ]

tri :: ComplexContour DontCare
tri = ComplexContour [
   AnnotatedVertex (Vertex3  75  75 0) 0,
   AnnotatedVertex (Vertex3 125 175 0) 0,
   AnnotatedVertex (Vertex3 175  75 0) 0 ]

rectAndTri :: ComplexPolygon DontCare
rectAndTri = ComplexPolygon [ rectangle, tri ]

star :: ComplexPolygon DontCare
star = ComplexPolygon [
   ComplexContour [
      AnnotatedVertex (Vertex3 250  50 0) 0,
      AnnotatedVertex (Vertex3 325 200 0) 0,
      AnnotatedVertex (Vertex3 400  50 0) 0,
      AnnotatedVertex (Vertex3 250 150 0) 0,
      AnnotatedVertex (Vertex3 400 150 0) 0 ] ]

myInit :: IO [DisplayList]
myInit = do
   clearColor $= Color4 0 0 0 0
   rectAndTriList <- compileNewList TessWindingOdd rectAndTri
   starList <- compileNewList TessWindingPositive star
   return [ rectAndTriList, starList ]

compileNewList :: TessWinding -> ComplexPolygon DontCare -> IO DisplayList
compileNewList windingRule complexPolygon =
   defineNewList Compile $
      drawTriangulation =<<
         triangulate windingRule 0 (Normal3 0 0 0) noOpCombiner complexPolygon

noOpCombiner :: Combiner DontCare
noOpCombiner _newVertex _weightedProperties = 0

drawTriangulation :: Triangulation DontCare -> IO ()
drawTriangulation (Triangulation triangles) =
   renderPrimitive Triangles $
      flip mapM_ triangles $ \(Triangle tv1 tv2 tv3) -> do
         randomColor
         drawTriangleVertex tv1
         drawTriangleVertex tv2
         drawTriangleVertex tv3

randomColor :: IO ()
randomColor = do
   r <- randomIO
   g <- randomIO
   b <- randomIO
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 r g b)

drawTriangleVertex :: TriangleVertex DontCare -> IO ()
drawTriangleVertex (AnnotatedVertex plainVertex (_, e)) = do
   edgeFlag $= e
   vertex plainVertex

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   createWindow progName
   displayLists <- myInit
   displayCallback $= display displayLists
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
