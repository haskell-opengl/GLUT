--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-- GLUT supports a number of callbacks to respond to events. There are three
-- types of callbacks: window, menu, and global. Window callbacks indicate when
-- to redisplay or reshape a window, when the visibility of the window changes,
-- and when input is available for the window. Menu callbacks are described in
-- "Graphics.UI.GLUT.Menu". The global callbacks manage the passing of time and
-- menu usage. The calling order of callbacks between different windows is
-- undefined.
--
-- Callbacks for input events should be delivered to the window the event occurs
-- in. Events should not propagate to parent windows.
--
-- A callback of type @Foo@ can registered by setting @fooCallback@ to 'Just'
-- the callback. Almost all callbacks can be de-registered by setting
-- the corresponding @fooCallback@ to 'Nothing', the only exceptions being
-- 'Graphics.UI.GLUT.Callbacks.Window.DisplayCallback' (can only be
-- re-registered) and 'Graphics.UI.GLUT.Callbacks.Global.TimerCallback' (can\'t
-- be unregistered).
--
-- /X Implementation Notes:/ The X GLUT implementation uses the X Input
-- extension to support sophisticated input devices: Spaceball, dial & button
-- box, and digitizing tablet. Because the X Input extension  does not mandate
-- how particular types of devices are advertised through the extension, it is
-- possible GLUT for X may not correctly support input devices that would
-- otherwise be of the correct type. The X GLUT implementation will support the
-- Silicon Graphics Spaceball, dial & button box, and digitizing tablet as
-- advertised through the X Input extension.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks (
   module Graphics.UI.GLUT.Callbacks.Window,
   module Graphics.UI.GLUT.Callbacks.Global
) where

import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Callbacks.Global
