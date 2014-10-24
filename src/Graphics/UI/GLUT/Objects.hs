--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Objects
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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
-- but do not generate texture coordinates (except for the solid teapot, teacup
-- and teaspoon). If VBOs should be used instead of the fixed function pipeline,
-- specify at least one of the attribute locations
-- 'Graphics.UI.GLUT.State.vertexAttribCoord3' or
-- 'Graphics.UI.GLUT.State.vertexAttribNormal'.
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

import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Raw

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
-- * The classic teaset modeled by Martin Newell in 1975. Both surface normals
--   and texture coordinates for the teaset are generated.
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
   | -- |(/freeglut only/) A teacup with a given relative size.
     Teacup Height
   | -- |(/freeglut only/) A teaspoon with a given relative size.
     Teaspoon Height
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
renderObject Solid     (Cube h)             = glutSolidCube h
renderObject Wireframe (Cube h)             = glutWireCube  h
renderObject Solid     Dodecahedron         = glutSolidDodecahedron
renderObject Wireframe Dodecahedron         = glutWireDodecahedron
renderObject Solid     Icosahedron          = glutSolidIcosahedron
renderObject Wireframe Icosahedron          = glutWireIcosahedron
renderObject Solid     Octahedron           = glutSolidOctahedron
renderObject Wireframe Octahedron           = glutWireOctahedron
renderObject Solid     Tetrahedron          = glutSolidTetrahedron
renderObject Wireframe Tetrahedron          = glutWireTetrahedron
renderObject Solid     RhombicDodecahedron  = glutSolidRhombicDodecahedron
renderObject Wireframe RhombicDodecahedron  = glutWireRhombicDodecahedron
renderObject Solid     (Sphere' r s t)      = glutSolidSphere r s t
renderObject Wireframe (Sphere' r s t)      = glutWireSphere  r s t
renderObject Solid     (Cone r h s t)       = glutSolidCone r h s t
renderObject Wireframe (Cone r h s t)       = glutWireCone  r h s t
renderObject Solid     (Cylinder' r h s t)  = glutSolidCylinder r h s t
renderObject Wireframe (Cylinder' r h s t)  = glutWireCylinder r h s t
renderObject Solid     (Torus i o s r)      = glutSolidTorus i o s r
renderObject Wireframe (Torus i o s r)      = glutWireTorus  i o s r
renderObject Solid     (Teapot h)           = glutSolidTeapot h
renderObject Wireframe (Teapot h)           = glutWireTeapot  h
renderObject Solid     (Teacup h)           = glutSolidTeacup h
renderObject Wireframe (Teacup h)           = glutWireTeacup  h
renderObject Solid     (Teaspoon h)         = glutSolidTeaspoon h
renderObject Wireframe (Teaspoon h)         = glutWireTeaspoon  h
renderObject Solid     (SierpinskiSponge n) = solidSierpinskiSponge n
renderObject Wireframe (SierpinskiSponge n) = wireSierpinskiSponge n

--------------------------------------------------------------------------------

solidSierpinskiSponge :: NumLevels -> IO ()
solidSierpinskiSponge = sierpinskiSponge glutSolidSierpinskiSponge

wireSierpinskiSponge :: NumLevels -> IO ()
wireSierpinskiSponge = sierpinskiSponge glutWireSierpinskiSponge

-- for consistency, we hide the offset and scale on the Haskell side
sierpinskiSponge :: (CInt -> Ptr GLdouble -> Height -> IO ()) -> NumLevels -> IO ()
sierpinskiSponge f n =
   with (Vertex3 0 0 0) $ \offsetBuf ->
      f (fromIntegral n) ((castPtr :: Ptr (Vertex3 GLdouble) -> Ptr GLdouble) offsetBuf) 1
