--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Menu
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GLUT supports simple cascading pop-up menus. They are designed to let a user
-- select various modes within a program. The functionality is simple and
-- minimalistic and is meant to be that way. Do not mistake GLUT\'s pop-up menu
-- facility with an attempt to create a full-featured user interface.
--
-- It is illegal to create or destroy menus, or change, add, or remove menu
-- items while a menu (and any cascaded sub-menus) are in use (that is, popped
-- up).
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Menu (
   MenuCB,
   createMenu, destroyMenu,
   setMenu, getMenu,
   addMenuEntry, addSubMenu,
   changeToMenuEntry, changeToSubMenu,
   removeMenuItem,
   attachMenu, detachMenu
) where

import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Foreign.Ptr ( FunPtr )
import Control.Monad ( liftM )

--------------------------------------------------------------------------------

-- | The type of a menu callback action that is called when a menu entry from a
-- menu is selected. The value passed to the callback is determined by the value
-- for the selected menu entry.

type MenuCB    = CInt -> IO ()

type MenuCBPtr = FunPtr MenuCB

type MenuID = CInt
type Button = CInt
type Value  = CInt
type Item   = CInt

--------------------------------------------------------------------------------

-- | Create a new pop-up menu and return a unique identifier for it, which can
-- be used when calling 'setMenu'. Implicitly, the /current menu/ is set to the
-- newly created menu.
--
-- When the menu callback is called because a menu entry is selected for the
-- menu, the /current menu/ will be implicitly set to the menu with the selected
-- entry before the callback is made.
--
-- /X Implementation Notes:/ If available, GLUT for X will take advantage of
-- overlay planes for implementing pop-up menus. The use of overlay planes can
-- eliminate display callbacks when pop-up menus are deactivated. The
-- @SERVER_OVERLAY_VISUALS@ convention is used to determine if overlay visuals
-- are available.

foreign import ccall unsafe "glutCreateMenu" createMenu :: MenuCBPtr -> IO MenuID

-- | Destroy the specified menu. If it was the /current menu/, the /current
-- menu/ becomes invalid and 'getMenu' will return 'Nothing'.

foreign import ccall unsafe "glutDestroyMenu" destroyMenu :: MenuID -> IO ()

-- | Set the /current menu./

foreign import ccall unsafe "glutSetMenu" setMenu :: MenuID -> IO ()

-- | Return 'Just' the identifier of the /current menu./ 'Nothing' is returned
-- if no menus exist or the previous /current menu/ was destroyed.

getMenu :: IO (Maybe MenuID)
getMenu = liftM (\i -> if i == 0 then Nothing else Just i) glutGetMenu

foreign import ccall unsafe "glutGetMenu" glutGetMenu :: IO MenuID

-- | Add a menu entry to the bottom of the /current menu./ The given string will
-- be displayed for the newly added menu entry. If the menu entry is selected by
-- the user, the menu\'s callback will be called passing the given value as the
-- callback\'s parameter.

addMenuEntry :: String -> Value -> IO ()
addMenuEntry name value = withCString name $ \n -> glutAddMenuEntry n value

foreign import ccall unsafe "glutAddMenuEntry" glutAddMenuEntry :: CString -> Value -> IO ()

-- | Add a sub-menu trigger to the bottom of the /current menu./ The given
-- string will be displayed for the newly added sub-menu trigger. If the
-- sub-menu trigger is entered, the sub-menu specified by the given menu
-- identifier will be cascaded, allowing sub-menu menu items to be selected.

addSubMenu :: String -> MenuID -> IO ()
addSubMenu name menuID = withCString name $ \n -> glutAddSubMenu n menuID

foreign import ccall unsafe "glutAddSubMenu" glutAddSubMenu :: CString -> MenuID -> IO ()

-- | Change the specified menu entry in the /current menu/ into a menu entry.
-- The given position determines which menu item should be changed and must be
-- between 1 (the topmost menu item) and
-- 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. The menu item to change
-- does not have to be a menu entry already. The given string will be displayed
-- for the newly changed menu entry. The given value will be returned to the
-- menu\'s callback if this menu entry is selected.

foreign import ccall unsafe "glutChangeToMenuEntry" changeToMenuEntry :: Item -> CString -> Value -> IO ()

-- | Change the specified menu item in the /current menu/ into a sub-menu
-- trigger. The  given position determines which menu item should be changed and
-- must be between 1 and 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. The
-- menu item to change does not have to be a sub-menu trigger already. The
-- given name will be displayed for the newly changed sub-menu trigger. The
-- given menu identifier names the sub-menu to cascade from the newly added
-- sub-menu trigger.

foreign import ccall unsafe "glutChangeToSubMenu" changeToSubMenu :: Item -> CString -> MenuID -> IO ()

-- | Remove the menu item at the given position, regardless of whether it is a
-- menu entry or sub-menu trigger. The position must be between 1 (the topmost
-- menu item) and 'Graphics.UI.GLUT.State.getNumMenuItems' inclusive. Menu items
-- below the removed menu item are renumbered.

foreign import ccall unsafe "glutRemoveMenuItem" removeMenuItem :: Item -> IO ()

-- | Attach a mouse button for the /current window/ to the identifier of the
-- /current menu./ By attaching a menu identifier to a button, the named menu
-- will be popped up when the user presses the specified button. Note that the
-- menu is attached to the button by identifier, not by reference.

foreign import ccall unsafe "glutAttachMenu" attachMenu :: Button -> IO ()

-- | Detach an attached mouse button from the /current window./
foreign import ccall unsafe "glutDetachMenu" detachMenu :: Button -> IO ()
