# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk
-include config.mk

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

PACKAGE = GLUT
PACKAGE_DEPS = base OpenGL

SRC_HC_OPTS += -Wall -fffi -Iinclude '-\#include "HsGLUT.h"' -cpp

# WinDoze DLL hell
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS += -DCALLCONV=stdcall
else
SRC_HC_OPTS += -DCALLCONV=ccall
endif

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries (GLUT package)"

# yeuch, have to get GLUT_CFLAGS & GLUT_LIBS in through CPP to package.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGLUT_CFLAGS='$(patsubst %,$(comma)"%",$(GLUT_CFLAGS))'
PACKAGE_CPP_OPTS += -DGLUT_LIBS='$(patsubst %,$(comma)"%",$(GLUT_LIBS))'
PACKAGE_CPP_OPTS += -DGLUT_EXTRA_LIBS='$(patsubst %,$(comma)"%",$(GLUT_EXTRA_LIBS))'
PACKAGE_CPP_OPTS += -DGLUT_FRAMEWORKS='"$(GLUT_FRAMEWORKS)"'

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

# -----------------------------------------------------------------------------

DIST_CLEAN_FILES += config.cache config.status config.mk

extraclean::
	$(RM) -rf autom4te.cache

# -----------------------------------------------------------------------------

endif

include $(TOP)/mk/target.mk
