# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

ifneq "$(findstring clean, $(MAKECMDGOALS))" ""
# if we're cleaning, then config.mk might have been cleaned already
GLUT_BUILD_PACKAGE=yes
PACKAGE=GLUT
endif

ifneq "$(GLUT_BUILD_PACKAGE)" "no"

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ifeq "$(IncludeExampleDirsInBuild)" "YES"
SUBDIRS += examples
endif

ALL_DIRS = \
	Graphics/UI \
	Graphics/UI/GLUT \
	Graphics/UI/GLUT/Callbacks

PACKAGE_DEPS = base OpenGL

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsGLUT.h"' -cpp

# WinDoze DLL hell
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS += -DCALLCONV=stdcall
else
SRC_HC_OPTS += -DCALLCONV=ccall
endif

PACKAGE_CPP_OPTS += -DMAINTAINER=$(MAINTAINER)

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries (GLUT package)"

# -----------------------------------------------------------------------------

package.conf.inplace \
package.conf.installed : include/HsGLUTConfig.h

Graphics/UI/GLUT/Begin.$(way_)o \
Graphics/UI/GLUT/Callbacks/Window.$(way_)o \
Graphics/UI/GLUT/Fonts.$(way_)o \
Graphics/UI/GLUT/Objects.$(way_)o \
Graphics/UI/GLUT/QueryUtils.$(way_)o : include/HsGLUTExt.h

# -----------------------------------------------------------------------------

STUBOBJS += \
   Graphics/UI/GLUT/Menu_stub.$(way_)o \
   Graphics/UI/GLUT/Callbacks/Global_stub.$(way_)o \
   Graphics/UI/GLUT/Callbacks/Registration_stub.$(way_)o \
   Graphics/UI/GLUT/Callbacks/Window_stub.$(way_)o

CLEAN_FILES += $(STUBOBJS) \
   Graphics/UI/GLUT/Menu_stub.[ch] \
   Graphics/UI/GLUT/Callbacks/Global_stub.[ch] \
   Graphics/UI/GLUT/Callbacks/Registration_stub.[ch] \
   Graphics/UI/GLUT/Callbacks/Window_stub.[ch]

endif

EXCLUDED_SRCS += Setup.hs

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += GLUT.buildinfo config.cache config.status config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
