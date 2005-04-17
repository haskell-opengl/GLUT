# -----------------------------------------------------------------------------

TOP = ../..
include $(TOP)/mk/boilerplate.mk
-include ../config.mk

# HACK
PACKAGE=

# -----------------------------------------------------------------------------

SRC_CC_OPTS += -Wall -I$(GHC_INCLUDE_DIR) -I../include $(GLUT_CFLAGS)

LIBRARY = libHSGLUT_cbits.a
LIBOBJS = $(C_OBJS)

# -----------------------------------------------------------------------------
# Per-module flags

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
