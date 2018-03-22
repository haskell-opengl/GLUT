{-
   Triangles.hs (adapted from triangles.cpp which is (c) The Red Book Authors.)
   Copyright (c) Sven Panne 2018 <svenpanne@gmail.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file GLUT/LICENSE

   Our first OpenGL program.
-}

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLUT
import Prelude hiding ( init )
import LoadShaders

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

init :: IO Descriptor
init = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        Vertex2 (-0.90) (-0.90),  -- Triangle 1
        Vertex2   0.85  (-0.90),
        Vertex2 (-0.90)   0.85 ,
        Vertex2   0.90  (-0.85),  -- Triangle 2
        Vertex2   0.90    0.90 ,
        Vertex2 (-0.85)   0.90 ] :: [Vertex2 GLfloat]
      numVertices = length vertices
      vertexSize = sizeOf (head vertices)

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "triangles.vert"),
     ShaderInfo FragmentShader (FileSource "triangles.frag")]
  currentProgram $= Just program

  let firstIndex = 0
      vPosition = AttribLocation 0
  vertexAttribPointer vPosition $=
    (ToFloat,
     VertexArrayDescriptor 2 Float 0 (bufferOffset (firstIndex * vertexSize)))
  vertexAttribArray vPosition $= Enabled

  return $
    Descriptor triangles (fromIntegral firstIndex) (fromIntegral numVertices)

display :: Descriptor -> DisplayCallback
display (Descriptor triangles firstIndex numVertices) = do
  clear [ ColorBuffer ]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  flush

main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ RGBAMode ]
  initialWindowSize $= Size 512 512
  initialContextVersion $= (4, 3)
  initialContextProfile $= [ CoreProfile ]
  _ <- createWindow progName
  descriptor <- init
  displayCallback $= display descriptor
  mainLoop
