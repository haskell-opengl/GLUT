# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2002/06/16 20:07:08 panne Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Graphics/UI \
	Graphics/UI/GLUT \
	Graphics/UI/GLUT/Callbacks

PACKAGE = GLUT

SRC_HC_OPTS += -Wall -fglasgow-exts -package OpenGL \
               -Iinclude '-\#include "HsGLUT.h"'

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to GLUT.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------

STUBOBJS += \
   Graphics/UI/GLUT/Callbacks/Window_stub.$(way_)o \
   Graphics/UI/GLUT/Callbacks/Global_stub.$(way_)o

CLEAN_FILES += $(STUBOBJS) \
   Graphics/UI/GLUT/Callbacks/Window_stub.[ch] \
   Graphics/UI/GLUT/Callbacks/Global_stub.[ch]

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
