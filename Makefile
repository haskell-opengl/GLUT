# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2002/05/26 19:46:13 panne Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Graphics/UI \
	Graphics/UI/GLUT

PACKAGE = GLUT

SRC_HC_OPTS += -fglasgow-exts '-\#include <GL/glut.h>' -package OpenGL -Iinclude

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to GLUT.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------
# Per-module flags

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
