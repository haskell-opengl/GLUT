# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2002/02/24 17:18:02 panne Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ALL_DIRS = \
	Graphics/UI \
	Graphics/UI/GLUT

PACKAGE = GLUT

SRC_HC_OPTS += -fglasgow-exts '-\#include <GL/glut.h>'

# yeuch, have to get GL_CFLAGS & GL_LIBS in through CPP to GLUT.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DGL_CFLAGS='$(patsubst %,$(comma)"%",$(GL_CFLAGS))'
PACKAGE_CPP_OPTS += -DGL_LIBS='$(patsubst %,$(comma)"%",$(GL_LIBS))'

# -----------------------------------------------------------------------------
# Per-module flags

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
