{-
   ColorTriangle.hs (adapted from triangles.cpp which is (c) The Red Book
   Authors.)
   Copyright (c) Sven Panne 2018 <svenpanne@gmail.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file GLUT/LICENSE

   A variation of Triangles.hs, adding colors to vertices via interleaved vertex
   attributes.
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

data ColoredVertex = ColoredVertex (Vertex2 GLfloat) (Color3 GLfloat)

instance Storable ColoredVertex where
  sizeOf ~(ColoredVertex v c) = sizeOf v + sizeOf c
  alignment ~(ColoredVertex v _) = alignment v
  peek ptr = do v <- peek (castPtr ptr)
                c <- peekByteOff (castPtr ptr) (sizeOf v)
                return $ ColoredVertex v c
  poke ptr (ColoredVertex v c) = do poke (castPtr ptr) v
                                    pokeByteOff (castPtr ptr) (sizeOf v) c

init :: IO Descriptor
init = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        -- Triangle 1
        ColoredVertex (Vertex2 (-0.90) (-0.90)) (Color3 1 0 0),
        ColoredVertex (Vertex2   0.85  (-0.90)) (Color3 0 1 0),
        ColoredVertex (Vertex2 (-0.90)   0.85 ) (Color3 0 0 1),
        -- Triangle 2
        ColoredVertex (Vertex2   0.90  (-0.85)) (Color3 0 1 1),
        ColoredVertex (Vertex2   0.90    0.90 ) (Color3 1 0 1),
        ColoredVertex (Vertex2 (-0.85)   0.90 ) (Color3 1 1 0)]
      numVertices = length vertices
      vertexSize = sizeOf (head vertices)

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "color_triangles.vert"),
     ShaderInfo FragmentShader (FileSource "color_triangles.frag")]
  currentProgram $= Just program

  let firstIndex = 0
      vPosition = AttribLocation 0
      vColor = AttribLocation 1
  vertexAttribPointer vPosition $=
    (ToFloat,
     VertexArrayDescriptor 2 Float (fromIntegral vertexSize)
                           (bufferOffset (firstIndex * vertexSize)))
  vertexAttribArray vPosition $= Enabled
  let colorOffset = case head vertices of ~(ColoredVertex v _) -> sizeOf v
  vertexAttribPointer vColor $=
    (ToFloat,
     VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
                           (bufferOffset ((firstIndex * vertexSize) +
                                          fromIntegral colorOffset)))
  vertexAttribArray vColor $= Enabled

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
  initialContextFlags $= [ DebugContext ]
  initialContextProfile $= [ CoreProfile ]
  _ <- createWindow progName
  descriptor <- init
  displayCallback $= display descriptor
  mainLoop
