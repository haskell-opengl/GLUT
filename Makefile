# -----------------------------------------------------------------------------

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

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

SRC_HADDOCK_OPTS += -t "HOpenGL Libraries (GLUT package)" -p prologue.txt

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

.PHONY: examples

examples:
	$(MAKE) -C examples

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
