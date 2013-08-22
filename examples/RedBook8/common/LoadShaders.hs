--------------------------------------------------------------------------------
-- |
-- Module      :  LoadShaders
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD-style (see the file GLUT/LICENSE)
--
-- Maintainer  :  svenpanne@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Utilities for shader handling, adapted from LoadShaders.cpp which is (c) The
-- Red Book Authors.
--
--------------------------------------------------------------------------------

module LoadShaders (
   ShaderSource(..), ShaderInfo(..), loadShaders
) where

import Control.Exception
import Control.Monad
import Graphics.Rendering.OpenGL

--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO String
getSource (FileSource path) = readFile path
getSource (StringSource src) = return src

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO Program
loadShaders infos =
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      loadCompileAttach program infos
      linkAndCheck program
      return program

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach :: Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
   createShader shType `bracketOnError` deleteObjectName $ \shader -> do
      src <- getSource source
      shaderSource shader $= [src]
      compileAndCheck shader
      attachShader program shader
      loadCompileAttach program infos

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)
