--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Objects
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- GLUT includes a number of routines for generating easily recognizable 3D
-- geometric objects. These routines reflect functionality available in the
-- @aux@ toolkit described in the /OpenGL Programmer\'s Guide/ and are included
-- in GLUT to allow the construction of simple GLUT programs that render
-- recognizable objects. These routines can be implemented as pure OpenGL
-- rendering routines. The routines do not generate display lists for the
-- objects they create. The routines generate normals appropriate for lighting
-- but do not generate texture coordinates (except for the teapot).
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Objects (
   -- * Rendering flavour
   Flavour(..),

   -- * Object description
   Object(..),

   -- * Type synonyms
   Sides, Rings, NumLevels,

   -- * Rendering
   renderObject
) where

import Foreign.C.Types ( CInt )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLdouble )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3(..) )
import Graphics.Rendering.OpenGL.GLU.Quadrics ( Radius, Height, Slices, Stacks )
import Graphics.UI.GLUT.Extensions

--------------------------------------------------------------------------------

#include "HsGLUTExt.h"

--------------------------------------------------------------------------------

-- | Flavour of object rendering

data Flavour
   = -- | Object is rendered as a solid with shading and surface normals.
     Solid
   | -- | Object is rendered as a wireframe without surface normals.
     Wireframe
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | GLUT offers five types of objects:
--
-- *  The five Platonic solids, see
--    <http://mathworld.wolfram.com/PlatonicSolid.html>.
--
-- * A rhombic dodecahedron, see
--   <http://mathworld.wolfram.com/RhombicDodecahedron.html>.
--
-- * Approximations to rounded objects.
--
-- * The classic teapot modeled by Martin Newell in 1975. Both surface normals
--   and texture coordinates for the teapot are generated. The teapot is
--   generated with OpenGL evaluators.
--
-- * A Sierpinski sponge, see
--   <http://mathworld.wolfram.com/Tetrix.html>.

data Object
   = -- | A cube centered at the modeling coordinates origin with sides of the
     --   given length.
     Cube Height
   | -- | A dodecahedron (12-sided regular solid) centered at the modeling
     --   coordinates origin with a radius of @sqrt 3@.
     Dodecahedron
   | -- | A icosahedron (20-sided regular solid) centered at the modeling
     --   coordinates origin with a radius of 1.0.
     Icosahedron
   | -- | Render a solid octahedron (8-sided regular solid) centered at the
     --   modeling coordinates origin with a radius of 1.0.
     Octahedron
   | -- | Render a solid tetrahedron (4-sided regular solid) centered at the
     --   modeling coordinates origin with a radius of @sqrt 3@.
     Tetrahedron
   | -- | (/freeglut only/) A rhombic dodecahedron whose corners are at most a
     -- distance of one from the origin. The rhombic dodecahedron has faces
     -- which are identical rhombi, but which have some vertices at which three
     -- faces meet and some vertices at which four faces meet. The length of
     -- each side is @(sqrt 3)\/2@. Vertices at which four faces meet are found
     -- at @(0, 0, +\/-1)@ and @(+\/-(sqrt 2)\/2, +\/-(sqrt 2)\/2, 0)@. 
     RhombicDodecahedron
   | -- | A sphere centered at the modeling coordinates origin of the specified
     --   radius. The sphere is subdivided around the Z axis into slices
     --   (similar to lines of longitude) and along the Z axis into stacks
     --   (similar to lines of latitude).
     Sphere' Radius Slices Stacks
   | -- | A cone oriented along the Z axis. The base of the cone is placed at Z
     --   = 0, and the top at Z = the given height. The cone is subdivided
     --   around the Z axis into slices, and along the Z axis into stacks.
     Cone Radius Height Slices Stacks
   | -- |(/freeglut only/) A cylinder oriented along the Z axis. The base of the
     --  cylinder is placed at Z = 0, and the top at Z = the given height. The
     --  cylinder is subdivided around the Z axis into slices, and along the Z
     -- axis into stacks.
     Cylinder' Radius Height Slices Stacks
   | -- | A torus (doughnut) centered at the modeling coordinates origin
     -- whose axis is aligned with the Z axis. The torus is described by its
     -- inner and outer radius, the number of sides for each radial section,
     -- and the number of radial divisions (rings).
     Torus Radius Radius Sides Rings
   | -- | A teapot with a given relative size.
     Teapot Height
   | -- |(/freeglut only/) A Sierpinski sponge of a given level, where a level
     -- 0 sponge is the same as a 'Tetrahedron'. 
     SierpinskiSponge NumLevels
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

type Sides     = GLint
type Rings     = GLint
type NumLevels = GLint

--------------------------------------------------------------------------------

-- | Render an object in the given flavour.

renderObject :: Flavour -> Object -> IO ()
renderObject Solid     (Cube h)             = solidCube h
renderObject Wireframe (Cube h)             = wireCube  h
renderObject Solid     Dodecahedron         = solidDodecahedron
renderObject Wireframe Dodecahedron         = wireDodecahedron
renderObject Solid     Icosahedron          = solidIcosahedron
renderObject Wireframe Icosahedron          = wireIcosahedron
renderObject Solid     Octahedron           = solidOctahedron
renderObject Wireframe Octahedron           = wireOctahedron
renderObject Solid     Tetrahedron          = solidTetrahedron
renderObject Wireframe Tetrahedron          = wireTetrahedron
renderObject Solid     RhombicDodecahedron  = glutSolidRhombicDodecahedron
renderObject Wireframe RhombicDodecahedron  = glutWireRhombicDodecahedron
renderObject Solid     (Sphere' r s t)      = solidSphere r s t
renderObject Wireframe (Sphere' r s t)      = wireSphere  r s t 
renderObject Solid     (Cone r h s t)       = solidCone r h s t
renderObject Wireframe (Cone r h s t)       = wireCone  r h s t
renderObject Solid     (Cylinder' r h s t)  = glutSolidCylinder r h s t
renderObject Wireframe (Cylinder' r h s t)  = glutWireCylinder r h s t
renderObject Solid     (Torus i o s r)      = solidTorus i o s r
renderObject Wireframe (Torus i o s r)      = wireTorus  i o s r 
renderObject Solid     (Teapot h)           = solidTeapot h
renderObject Wireframe (Teapot h)           = wireTeapot  h
renderObject Solid     (SierpinskiSponge n) = solidSierpinskiSponge n
renderObject Wireframe (SierpinskiSponge n) = wireSierpinskiSponge n

--------------------------------------------------------------------------------

-- | Render a solid cube centered at the modeling coordinates origin with sides
-- of the given length.

foreign import CALLCONV unsafe "glutSolidCube" solidCube
   :: Height -- ^ Length of the cube sides
   -> IO ()

-- | Render a wireframe cube centered at the modeling coordinates origin with
-- sides of the given length.

foreign import CALLCONV unsafe "glutWireCube" wireCube
   :: Height -- ^ Length of the cube sides
   -> IO ()

--------------------------------------------------------------------------------

-- | Render a solid dodecahedron (12-sided regular solid) centered at the
-- modeling coordinates origin with a radius of @sqrt 3@.

foreign import CALLCONV unsafe "glutSolidDodecahedron" solidDodecahedron ::
   IO ()

-- | Render a wireframe dodecahedron (12-sided regular solid) centered at the
-- modeling coordinates origin with a radius of @sqrt 3@.

foreign import CALLCONV unsafe "glutWireDodecahedron" wireDodecahedron :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid icosahedron (20-sided regular solid) centered at the
-- modeling coordinates origin with a radius of 1.0.

foreign import CALLCONV unsafe "glutWireIcosahedron" wireIcosahedron :: IO ()

-- | Render a wireframe icosahedron (20-sided regular solid) centered at the
-- modeling coordinates origin with a radius of 1.0.

foreign import CALLCONV unsafe "glutSolidIcosahedron" solidIcosahedron :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid octahedron (8-sided regular solid) centered at the modeling
-- coordinates origin with a radius of 1.0.

foreign import CALLCONV unsafe "glutSolidOctahedron" solidOctahedron :: IO ()

-- | Render a wireframe octahedron (8-sided regular solid) centered at the
-- modeling coordinates origin with a radius of 1.0.

foreign import CALLCONV unsafe "glutWireOctahedron" wireOctahedron :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid tetrahedron (4-sided regular solid) centered at the modeling
-- coordinates origin with a radius of @sqrt 3@.

foreign import CALLCONV unsafe "glutWireTetrahedron" wireTetrahedron :: IO ()

-- | Render a wireframe tetrahedron (4-sided regular solid) centered at the
-- modeling coordinates origin with a radius of @sqrt 3@.

foreign import CALLCONV unsafe "glutSolidTetrahedron" solidTetrahedron  :: IO ()

--------------------------------------------------------------------------------

EXTENSION_ENTRY(unsafe,"freeglut",glutSolidRhombicDodecahedron,IO ())

EXTENSION_ENTRY(unsafe,"freeglut",glutWireRhombicDodecahedron,IO ())

--------------------------------------------------------------------------------

-- | Render a solid sphere centered at the modeling coordinates origin of the
-- specified radius. The sphere is subdivided around the Z axis into slices
-- and along the Z axis into stacks.

foreign import CALLCONV unsafe "glutSolidSphere" solidSphere
   :: Radius   -- ^ Radius of the sphere.
   -> Slices   -- ^ Number of subdivisions (slices) around the Z axis, similar
               --   to lines of longitude.
   -> Stacks   -- ^ The number of subdivisions (stacks) along the Z axis,
               --   similar to lines of latitude.
   -> IO ()

-- | Render a wireframe sphere centered at the modeling coordinates origin of
-- the specified radius. The sphere is subdivided around the Z axis into slices
-- and along the Z axis into stacks.

foreign import CALLCONV unsafe "glutWireSphere" wireSphere
   :: Radius   -- ^ Radius of the sphere.
   -> Slices   -- ^ Number of subdivisions (slices) around the Z axis, similar
               --   to lines of longitude.
   -> Stacks   -- ^ The number of subdivisions (stacks) along the Z axis,
               --   similar to lines of latitude.
   -> IO ()

--------------------------------------------------------------------------------

-- | Render a solid cone oriented along the Z axis. The base of the cone is
-- placed at Z = 0, and the top at Z = height. The cone is subdivided around the
-- Z axis into slices, and along the Z axis into stacks.

foreign import CALLCONV unsafe "glutSolidCone" solidCone
   :: Radius   -- ^ Radius of the base of the cone.
   -> Height   -- ^ Height of the cone.
   -> Slices   -- ^ Number of subdivisions around the Z axis.
   -> Stacks   -- ^ The number of subdivisions along the Z axis.
   -> IO ()

-- | Render a wireframe cone oriented along the Z axis. The base of the cone is
-- placed at Z = 0, and the top at Z = height. The cone is subdivided around the
-- Z axis into slices, and along the Z axis into stacks.

foreign import CALLCONV unsafe "glutWireCone" wireCone
   :: Radius   -- ^ Radius of the base of the cone.
   -> Height   -- ^ Height of the cone.
   -> Slices   -- ^ Number of subdivisions around the Z axis.
   -> Stacks   -- ^ The number of subdivisions along the Z axis.
   -> IO ()

--------------------------------------------------------------------------------

EXTENSION_ENTRY(unsafe,"freeglut",glutSolidCylinder,Radius -> Height -> Slices -> Stacks -> IO ())

EXTENSION_ENTRY(unsafe,"freeglut",glutWireCylinder,Radius -> Height -> Slices -> Stacks -> IO ())

--------------------------------------------------------------------------------

-- | Render a solid torus (doughnut) centered at the modeling coordinates origin
-- whose axis is aligned with the Z axis.

foreign import CALLCONV unsafe "glutSolidTorus" solidTorus
   :: Radius   -- ^ Inner radius of the torus.
   -> Radius   -- ^ Outer radius of the torus.
   -> Slices   -- ^ Number of sides for each radial section.
   -> Stacks   -- ^ Number of radial divisions for the torus.
   -> IO ()

-- | Render a wireframe torus (doughnut) centered at the modeling coordinates
-- origin whose axis is aligned with the Z axis.

foreign import CALLCONV unsafe "glutWireTorus" wireTorus
   :: Radius   -- ^ Inner radius of the torus.
   -> Radius   -- ^ Outer radius of the torus.
   -> Slices   -- ^ Number of sides for each radial section.
   -> Stacks   -- ^ Number of radial divisions for the torus.
   -> IO ()

--------------------------------------------------------------------------------

-- | Render a solid teapot.

foreign import CALLCONV unsafe "glutSolidTeapot" solidTeapot  
   :: Height -- ^ Relative size of the teapot
   -> IO ()

-- | Render a wireframe teapot.

foreign import CALLCONV unsafe "glutWireTeapot" wireTeapot
   :: Height -- ^ Relative size of the teapot
   -> IO ()

--------------------------------------------------------------------------------

solidSierpinskiSponge :: NumLevels -> IO ()
solidSierpinskiSponge = sierpinskiSponge glutSolidSierpinskiSponge

EXTENSION_ENTRY(unsafe,"freeglut",glutSolidSierpinskiSponge,CInt -> Ptr (Vertex3 GLdouble) -> Height -> IO ())

wireSierpinskiSponge :: NumLevels -> IO ()
wireSierpinskiSponge = sierpinskiSponge glutWireSierpinskiSponge

EXTENSION_ENTRY(unsafe,"freeglut",glutWireSierpinskiSponge,CInt -> Ptr (Vertex3 GLdouble) -> Height -> IO ())

-- for consistency, we hide the offset and scale on the Haskell side
sierpinskiSponge :: (CInt -> Ptr (Vertex3 GLdouble) -> Height -> IO ()) -> NumLevels -> IO ()
sierpinskiSponge f n =
   with (Vertex3 0 0 0) $ \offsetBuf ->
      f (fromIntegral n) offsetBuf 1
