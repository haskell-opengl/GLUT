-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Initialization
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Actions beginning with the @init@ prefix are used to initialize GLUT
-- state. The primary initialization routine is 'init' that should only be
-- called exactly once in a GLUT program.  No non-@init@-prefixed GLUT or
-- OpenGL actions should be called before 'init'.
--
-- The other @init@-actions may be called before 'init'. The reason is these
-- actions can be used to set default window initialization state that might
-- be modified by the command processing done in 'init'. For example,
-- @'initWindowSize' ('WindowSize' 400 400)@ can be called before 'init' to
-- indicate 400 by 400 is the program\'s default window size. Setting the
-- initial window size or position before 'init' allows the GLUT program user
-- to specify the initial size or position using command line arguments.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Initialization (
   -- * Primary initialization
   init, initArgs,

   -- * Setting the initial window geometry
   WindowPosition(..), initWindowPosition,
   WindowSize(..), initWindowSize,

   -- * Setting the initial window mode
   DisplayMode(..), initDisplayMode, initDisplayString
) where

import Prelude hiding ( init )
import Data.Bits ( Bits((.|.)) )
import Data.List ( genericLength, intersperse )
import Foreign.C.String ( CString, withCString, peekCString )
import Foreign.C.Types ( CInt, CUInt )
import Foreign.Marshal.Array ( withArray0, peekArray )
import Foreign.Marshal.Utils ( withObject, withMany )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Storable ( Storable(..) )
import System.Environment ( getProgName, getArgs )
import Graphics.UI.GLUT.Constants

-----------------------------------------------------------------------------
-- | Given the program name and command line arguments, initialize the GLUT
-- library and negotiate a session with the window system. During this
-- process, 'init' may cause the termination of the GLUT program with an
-- error message to the user if GLUT cannot be properly initialized.
-- Examples of this situation include the failure to connect to the window
-- system, the lack of window system support for OpenGL, and invalid command
-- line options.
--
-- 'init' also processes command line options, but the specific options
-- parsed are window system dependent. Any command line arguments which are
-- not GLUT-specific are returned.
--
-- X Implementation Notes: The X Window System specific options parsed by
-- 'init' are as follows:
--
-- * @-display /DISPLAY/@: Specify the X server to connect to. If not specified,
--   the value of the @DISPLAY@ environment variable is used.
--
-- * @-geometry /WxH+X+Y/@: Determines where windows should be created on the
--   screen. The parameter following @-geometry@ should be formatted as a
--   standard X geometry specification. The effect of using this option is to
--   change the GLUT initial size and initial position the same as if
--   'initWindowSize' or 'initWindowPosition' were called directly.
--
-- * @-iconic@: Requests all top-level windows be created in an iconic state.
--
-- * @-indirect@: Force the use of indirect OpenGL rendering contexts.
--
-- * @-direct@: Force the use of direct OpenGL rendering contexts (not all GLX
--   implementations support direct rendering contexts). A fatal error is
--   generated if direct rendering is not supported by the OpenGL
--   implementation. If neither @-indirect@ or @-direct@ are used to force a
--   particular behavior, GLUT will attempt to use direct rendering if
--   possible and otherwise fallback to indirect rendering.
--
-- * @-gldebug@: After processing callbacks and\/or events, check if there
--   are any OpenGL errors by calling 'getError'. If an error is reported,
--   print out a warning by looking up the error code with 'errorString'.
--   Using this option is helpful in detecting OpenGL run-time errors.
--
-- * @-sync@: Enable synchronous X protocol transactions. This option makes
--   it easier to track down potential X protocol errors.

init :: String      -- @ The program name.
     -> [String]    -- @ The command line arguments
     -> IO [String] -- @ Non-GLUT command line arguments
init prog args =
   withObject (1 + genericLength args) $ \argcBuf ->
   withMany withCString (prog : args) $ \argvPtrs ->
   withArray0 nullPtr argvPtrs $ \argvBuf -> do
   glutInit argcBuf argvBuf
   newArgc <- peek argcBuf
   newArgvPtrs <- peekArray (fromIntegral newArgc) argvBuf
   newArgv <- mapM peekCString argvPtrs
   return $ tail newArgv

foreign import ccall unsafe "glutInit" glutInit ::
   Ptr CInt -> Ptr CString -> IO ()

-- | Convenience action: Initialize GLUT, returning the program name and any
-- non-GLUT command line arguments.

initArgs :: IO (String, [String])
initArgs = do
   prog <- getProgName
   args <- getArgs
   nonGLUTArgs <- init prog args
   return (prog, nonGLUTArgs)

-----------------------------------------------------------------------------

-- | Window position, measured in pixels.
data WindowPosition = WindowPosition CInt CInt

-- | Set the /initial window position/.  Windows created by
-- 'Graphics.UI.GLUT.Window.createWindow' will be requested to be created with
-- the current /initial window position/. The initial value of the /initial
-- window position/ GLUT state is @'WindowPosition' (-1) (-1)@. If either the
-- X or Y component of the /initial window position/ is negative, the actual
-- window position is left to the window system to determine.
--
-- The intent of the /initial window position/ is to provide a suggestion to
-- the window system for a window\'s initial position. The window system is
-- not obligated to use this information. Therefore, GLUT programs should not
-- assume the window was created at the specified position.

initWindowPosition :: WindowPosition -> IO ()
initWindowPosition (WindowPosition x y) = glutInitWindowPosition x y

foreign import ccall unsafe "glutInitWindowPosition" glutInitWindowPosition
   :: CInt -> CInt -> IO ()

-- | Window size, measured in pixels.
data WindowSize = WindowSize CInt CInt

-- | Set the /initial window size/.  Windows created by
-- 'Graphics.UI.GLUT.Window.createWindow' will be requested to be created with
-- the current /initial window size/. The initial value of the /initial window
-- size/ GLUT state is @'WindowPosition' 300 300@. The /initial window size/
-- components must be greater than zero.
--
-- The intent of the /initial window size/ is to provide a suggestion to the
-- window system for a window\'s initial size. The window system is not
-- obligated to use this information. Therefore, GLUT programs should not
-- assume the window was created at the specified size. A GLUT program should
-- use the window\'s reshape callback to determine the true size of the
-- window.

initWindowSize :: WindowSize -> IO ()
initWindowSize (WindowSize w h) = glutInitWindowSize w h

foreign import ccall unsafe "glutInitWindowSize" glutInitWindowSize ::
   CInt -> CInt -> IO ()

-----------------------------------------------------------------------------

-- | A single aspect of a window which is to be created.
data DisplayMode
   = Rgba        -- ^ Select an RGBA mode window. This is the default if neither 'Rgba' nor 'Index' are specified.
   | Rgb         -- ^ An alias for 'Rgba'.
   | Index       -- ^ Select a color index mode window. This overrides 'Rgba' if it is also specified.
   | Single      -- ^ Select a single buffered window. This is the default if neither 'Double' nor 'Single' are specified.
   | Double      -- ^ Select a double buffered window. This overrides 'Single' if it is also specified.
   | Accum       -- ^ Select a window with an accumulation buffer.
   | Alpha       -- ^ Select a window with an alpha component to the color buffer(s).
   | Depth       -- ^ Select a window with a depth buffer.
   | Stencil     -- ^ Select a window with a stencil buffer.
   | Multisample -- ^ Select a window with multisampling support. If multisampling is not available, a non-multisampling
                 --   window will automatically be chosen. Note: both the OpenGL client-side and server-side implementations
                 --   must support the @GLX_SAMPLE_SGIS@ extension for multisampling to be available.
   | Stereo      -- ^ Select A Stereo Window.
   | Luminance   -- ^ Select a window with a \"luminance\" color model. This model provides the functionality of OpenGL\'s
                 --   RGBA color model, but the green and blue components are not maintained in the frame buffer. Instead
                 --   each pixel\'s red component is converted to an index between zero and  @'get' 'WindowColormapSize' -1@
                 --   and looked up in a per-window color map to determine the color of pixels within the window. The initial
                 --   colormap of 'Luminance' windows is initialized to be a linear gray ramp, but can be modified with GLUT\'s
                 --   colormap actions. Implementation Notes: 'Luminance' is not supported on most OpenGL platforms.
   deriving ( Eq, Ord )

marshalDisplayMode :: DisplayMode -> CUInt
marshalDisplayMode m = case m of
   Rgba        -> glut_RGBA
   Rgb         -> glut_RGB
   Index       -> glut_INDEX
   Single      -> glut_SINGLE
   Double      -> glut_DOUBLE
   Accum       -> glut_ACCUM
   Alpha       -> glut_ALPHA
   Depth       -> glut_DEPTH
   Stencil     -> glut_STENCIL
   Multisample -> glut_MULTISAMPLE
   Stereo      -> glut_STEREO
   Luminance   -> glut_LUMINANCE

-- | Set the /initial display mode/ used when creating top-level windows,
-- subwindows, and overlays to determine the OpenGL display mode for the
-- to-be-created window or overlay.
--
-- Note that 'Rgba' selects the RGBA color model, but it does not request any
-- bits of alpha (sometimes called an /alpha buffer/ or /destination alpha/)
-- be allocated. To request alpha, specify 'Alpha'. The same applies to
-- 'Luminance'.

initDisplayMode :: [DisplayMode] -> IO ()
initDisplayMode = glutInitDisplayMode . toBitfield marshalDisplayMode

toBitfield :: (Bits b) => (a -> b) -> [a] -> b
toBitfield marshal = foldl (.|.) 0 . map marshal

foreign import ccall unsafe "glutInitDisplayMode" glutInitDisplayMode ::
   CUInt -> IO ()

-- | Set the /initial display mode description string/. It is used when
-- creating top-level windows, subwindows, and overlays to determine the
-- OpenGL display mode for the to-be-created window or overlay. The string is
-- a list of zero or more capability descriptions separated by spaces and
-- tabs. Each capability description is a capability name that is optionally
-- followed by a comparator and a numeric value. For example, @\"double\"@
-- and @\"depth>=12\"@ are both valid criteria. The capability descriptions
-- are translated into a set of criteria used to select the appropriate frame
-- buffer configuration. The criteria are matched in strict left to right
-- order of precdence. That is, the first specified criteria (leftmost) takes
-- precedence over the later criteria for non-exact criteria (greater than,
-- less than, etc. comparators). Exact criteria (equal, not equal
-- comparators) must match exactly so precedence is not relevant. The numeric
-- value is an integer that is parsed according to ANSI C\'s @strtol@
-- behavior. This means that decimal, octal (leading @0@), and hexadecimal
-- values (leading @0x@) are accepeted. The valid comparators are:
--
-- * @=@: Equal.
--
-- * @!=@: Not equal.
--
-- * @\<@: Less than and preferring larger difference (the least is best).
--
-- * @>@: Greater than and preferring larger differences (the most is best).
--
-- * @\<=@: Less than or equal and preferring larger difference (the least is
--   best).
--
-- * @>=@: Greater than or equal and preferring more instead of less. This
--   comparator is useful for allocating resources like color precision or
--   depth buffer precision where the maximum precision is generally
--   preferred. Contrast with the tilde (@~@) comparator.
--
-- * @~@: Greater than or equal but preferring less instead of more. This
--   comparator is useful for allocating resources such as stencil bits or
--   auxillary color buffers where you would rather not over-allocate. When
--   the comparator and numeric value are not specified, each capability name
--   has a different default (one default is to require a a comparator and
--   numeric value).
--
-- The valid capability names are:
--
-- * @rgba@: Number of bits of red, green, blue, and alpha in the RGBA color
--   buffer. Default is @\">=1\"@ for red, green, blue, and alpha
--   capabilities, and @\"=1\"@ for the RGBA color model capability.
--
-- * @rgb@: Number of bits of red, green, and blue in the RGBA color buffer
--   and zero bits of alpha color buffer precision. Default is @\">=1\"@ for
--   the red, green, and blue capabilities, and @\"~0\"@ for alpha
--   capability, and @\"=1\"@ for the RGBA color model capability.
--
-- * @red@: Red color buffer precision in bits. Default is @\">=1\"@.
--
-- * @green@: Green color buffer precision in bits. Default is @\">=1\"@.
--
-- * @blue@: Blue color buffer precision in bits. Default is @\">=1\"@.
--
-- * @alpha@: Alpha color buffer precision in bits. Default is @\">=1\"@.
--
-- * @index@: Boolean if the color model is color index or not. True is color
--   index. Default is @\">=1\"@.
--
-- * @buffer@: Number of bits in the color index color buffer. Default is
--   @\">=1\"@.
--
-- * @acca@: Red, green, blue, and alpha accumulation buffer precision in
--   bits. Default is @\">=1\"@ for red, green, blue, and alpha capabilities.
--
-- * @acc@: Red, green, and green accumulation buffer precision in bits and
--   zero bits of alpha accumulation buffer precision. Default is @\">=1\"@
--   for red, green, and blue capabilities, and @\"~0\"@ for the alpha
--   capability.
--
-- * @depth@: Number of bits of precsion in the depth buffer. Default is
--   @\">=12\"@.
--
-- * @stencil@: Number of bits in the stencil buffer.
--
-- * @luminance@: Number of bits of red in the RGBA and zero bits of green,
--   blue (alpha not specified) of color buffer precision. Default is
--   @\">=1\" for the red capabilitis, and @\"=0\" for the green and blue
--   capabilities, and @\"=1\" for the RGBA color model capability, and, for
--   X11, @\"=1\" for the @StaticGray@ (@\"xstaticgray\"@) capability. SGI
--   InfiniteReality (and other future machines) support a 16-bit luminance
--   (single channel) display mode (an additional 16-bit alpha channel can
--   also be requested). The red channel maps to gray scale and green and
--   blue channels are not available. A 16-bit precision luminance display
--   mode is often appropriate for medical imaging applications. Do not
--   expect many machines to support extended precision luminance display
--   modes.
--
-- * @single@: Boolean indicate the color buffer is single buffered. Double
--   buffer capability @\"=1\"@.
--
-- * @double@: Boolean indicating if the color buffer is double buffered.
--   Default is @\"=1\"@.
--
-- * @stereo@: Boolean indicating the color buffer is supports OpenGL-style
--   stereo. Default is @\"=1\"@.
--
-- * @samples@: Indicates the number of multisamples to use based on GLX\'s
--   @SGIS_multisample@ extension (for antialiasing). Default is @\"\<=4\"@.
--   This default means that a GLUT application can request multisampling if
--   available by simply specifying @\"samples\"@.
--
-- * @num@: A special capability name indicating where the value represents
--   the Nth frame buffer configuration matching the description string. When
--   not specified, 'initDisplayString' also returns the first (best
--   matching) configuration. @num@ requires a comparator and numeric value.
--
-- * @conformant@: Boolean indicating if the frame buffer configuration is
--   conformant or not. Conformance information is based on GLX\'s
--   @EXT_visual_rating@ extension if supported. If the extension is not
--   supported, all visuals are assumed conformat. Default is @\"=1\"@.
--
-- * @slow@: Boolean indicating if the frame buffer configuration is slow or
--   not. Slowness information is based on GLX\'s @EXT_visual_rating@
--   extension if supported. If the extension is not supported, all visuals
--   are assumed fast. Note that slowness is a relative designation relative
--   to other frame buffer configurations available. The intent of the slow
--   capability is to help programs avoid frame buffer configurations that
--   are slower (but perhaps higher precision) for the current machine.
--   Default is @\">=0\"@. This default means that slow visuals are used in
--   preference to fast visuals, but fast visuals will still be allowed.
--
-- * @win32pfd@: Only recognized on GLUT implementations for Win32, this
--   capability name matches the Win32 Pixel Format Descriptor by number.
--   @win32pfd@ requires a comparator and numeric value.
--
-- * @xvisual@: Only recongized on GLUT implementations for the X Window
--   System, this capability name matches the X visual ID by number.
--   @xvisual@ requires a comparator and numeric value.
--
-- * @xstaticgray@: Only recongized on GLUT implementations for the X Window
--   System, boolean indicating if the frame buffer configuration\'s X visual
--   is of type @StaticGray@. Default is @\"=1\"@.
--
-- * @xgrayscale@: Only recongized on GLUT implementations for the X Window
--   System, boolean indicating if the frame buffer configuration\'s X visual
--   is of type @GrayScale@. Default is @\"=1\"@.
--
-- * @xstaticcolor@: Only recongized on GLUT implementations for the X Window
--   System, boolean indicating if the frame buffer configuration\'s X visual
--   is of type @StaticColor@. Default is @\"=1\"@.
--
-- * @xpseudocolor@: Only recongized on GLUT implementations for the X Window
--   System, boolean indicating if the frame buffer configuration\'s X visual
--   is of type @PsuedoColor@. Default is @\"=1\"@.
--
-- * @xtruecolor@: Only recongized on GLUT implementations for the X Window
--   System, boolean indicating if the frame buffer configuration\'s X visual
--   is of type @TrueColor@. Default is @\"=1\"@.
--
-- * @xdirectcolor@: Only recongized on GLUT implementations for the X Window
--   System, boolean indicating if the frame buffer configuration\'s X visual
--   is of type @DirectColor@. Default is @\"=1\"@.
--
-- Unspecified capability descriptions will result in unspecified criteria
-- being generated. These unspecified criteria help 'initDisplayString'
-- behave sensibly with terse display mode description strings.
--
-- Here is an example using 'initDisplayString':
--
-- @   'initDisplayString' \"stencil~2 rgb double depth>=16 samples\"@
--
-- The above call requests a window with an RGBA color model (but requesting
-- no bits of alpha), a depth buffer with at least 16 bits of precision but
-- preferring more, multisampling if available, and at least 2 bits of
-- stencil (favoring less stencil to more as long as 2 bits are available).  

initDisplayString :: String -> IO ()
initDisplayString s = withCString s glutInitDisplayString

foreign import ccall unsafe "glutInitDisplayString" glutInitDisplayString ::
  CString -> IO ()
