--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Objects
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
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
   -- * Platonic solids

   -- $PlatonicSolids
   solidCube, wireCube,
   solidDodecahedron, wireDodecahedron,
   solidIcosahedron, wireIcosahedron,
   solidOctahedron, wireOctahedron,
   solidTetrahedron, wireTetrahedron,

   -- * Rounded objects
   solidSphere, wireSphere,
   solidCone, wireCone,
   solidTorus, wireTorus,

   -- * Newell\'s teapot

   -- $NewellsTeapot
   solidTeapot, wireTeapot
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLdouble, GLint )

--------------------------------------------------------------------------------
-- $PlatonicSolids
-- The following routines render the five Platonic solids (see
-- <http://mathworld.wolfram.com/PlatonicSolid.html>), either in solid or
-- wireframe form.

--------------------------------------------------------------------------------

-- | Render a solid cube centered at the modeling coordinates origin with sides
-- of the given length.

foreign import ccall unsafe "glutSolidCube" solidCube
   :: GLdouble -- ^ Length of the cube sides
   -> IO ()

-- | Render a wireframe cube centered at the modeling coordinates origin with sides
-- of the given length.

foreign import ccall unsafe "glutWireCube" wireCube
   :: GLdouble -- ^ Length of the cube sides
   -> IO ()

--------------------------------------------------------------------------------

-- | Render a solid dodecahedron (12-sided regular solid) centered at the
-- modeling coordinates origin with a radius of @sqrt 3@.

foreign import ccall unsafe "glutSolidDodecahedron" solidDodecahedron :: IO ()

-- | Render a wireframe dodecahedron (12-sided regular solid) centered at the
-- modeling coordinates origin with a radius of @sqrt 3@.

foreign import ccall unsafe "glutWireDodecahedron" wireDodecahedron :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid icosahedron (20-sided regular solid) centered at the
-- modeling coordinates origin with a radius of 1.0.

foreign import ccall unsafe "glutWireIcosahedron" wireIcosahedron :: IO ()

-- | Render a wireframe icosahedron (20-sided regular solid) centered at the
-- modeling coordinates origin with a radius of 1.0.

foreign import ccall unsafe "glutSolidIcosahedron" solidIcosahedron :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid octahedron (8-sided regular solid) centered at the modeling
-- coordinates origin with a radius of 1.0.

foreign import ccall unsafe "glutSolidOctahedron" solidOctahedron :: IO ()

-- | Render a wireframe octahedron (8-sided regular solid) centered at the
-- modeling coordinates origin with a radius of 1.0.

foreign import ccall unsafe "glutWireOctahedron" wireOctahedron :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid tetrahedron (4-sided regular solid) centered at the modeling
-- coordinates origin with a radius of @sqrt 3@.

foreign import ccall unsafe "glutWireTetrahedron"   wireTetrahedron   :: IO ()

-- | Render a wireframe tetrahedron (4-sided regular solid) centered at the
-- modeling coordinates origin with a radius of @sqrt 3@.

foreign import ccall unsafe "glutSolidTetrahedron"  solidTetrahedron  :: IO ()

--------------------------------------------------------------------------------

-- | Render a solid sphere centered at the modeling coordinates origin of the
-- specified radius. The sphere is subdivided around the Z axis into slices
-- and along the Z axis into stacks.

foreign import ccall unsafe "glutSolidSphere" solidSphere
   :: GLdouble -- ^ Radius of the sphere.
   -> GLint    -- ^ Number of subdivisions (slices) around the Z axis, similar to lines of longitude.
   -> GLint    -- ^ The number of subdivisions (stacks) along the Z axis, similar to lines of latitude.
   -> IO ()

-- | Render a wireframe sphere centered at the modeling coordinates origin of the
-- specified radius. The sphere is subdivided around the Z axis into slices
-- and along the Z axis into stacks.

foreign import ccall unsafe "glutWireSphere" wireSphere
   :: GLdouble -- ^ Radius of the sphere.
   -> GLint    -- ^ Number of subdivisions (slices) around the Z axis, similar to lines of longitude.
   -> GLint    -- ^ The number of subdivisions (stacks) along the Z axis, similar to lines of latitude.
   -> IO ()

--------------------------------------------------------------------------------

-- | Render a solid cone oriented along the Z axis. The base of the cone is
-- placed at Z = 0, and the top at Z = height. The cone is subdivided around the
-- Z axis into slices, and along the Z axis into stacks.

foreign import ccall unsafe "glutSolidCone" solidCone
   :: GLdouble -- ^ Radius of the base of the cone.
   -> GLdouble -- ^ Height of the cone.
   -> GLint    -- ^ Number of subdivisions around the Z axis.
   -> GLint    -- ^ The number of subdivisions along the Z axis.
   -> IO ()

-- | Render a wireframe cone oriented along the Z axis. The base of the cone is
-- placed at Z = 0, and the top at Z = height. The cone is subdivided around the
-- Z axis into slices, and along the Z axis into stacks.

foreign import ccall unsafe "glutWireCone" wireCone
   :: GLdouble -- ^ Radius of the base of the cone.
   -> GLdouble -- ^ Height of the cone.
   -> GLint    -- ^ Number of subdivisions around the Z axis.
   -> GLint    -- ^ The number of subdivisions along the Z axis.
   -> IO ()

--------------------------------------------------------------------------------

-- | Render a solid torus (doughnut) centered at the modeling coordinates origin
-- whose axis is aligned with the Z axis.

foreign import ccall unsafe "glutSolidTorus" solidTorus
   :: GLdouble -- ^ Inner radius of the torus.
   -> GLdouble -- ^ Outer radius of the torus.
   -> GLint    -- ^ Number of sides for each radial section.
   -> GLint    -- ^ Number of radial divisions for the torus.
   -> IO ()

-- | Render a wireframe torus (doughnut) centered at the modeling coordinates
-- origin whose axis is aligned with the Z axis.

foreign import ccall unsafe "glutWireTorus" wireTorus
   :: GLdouble -- ^ Inner radius of the torus.
   -> GLdouble -- ^ Outer radius of the torus.
   -> GLint    -- ^ Number of sides for each radial section.
   -> GLint    -- ^ Number of radial divisions for the torus.
   -> IO ()

--------------------------------------------------------------------------------
-- $NewellsTeapot
-- The following routines render the classic teapot modeled by Martin Newell in
-- 1975. Both surface normals and texture coordinates for the teapot are
-- generated. The teapot is generated with OpenGL evaluators.

-- | Render a solid teapot.

foreign import ccall unsafe "glutSolidTeapot" solidTeapot  
   :: GLdouble -- ^ Relative size of the teapot
   -> IO ()

-- | Render a wireframe teapot.

foreign import ccall unsafe "glutWireTeapot" wireTeapot
   :: GLdouble -- ^ Relative size of the teapot
   -> IO ()
