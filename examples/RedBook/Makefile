# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2003/03/02 19:30:20 panne Exp $

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

EXAMPLES    := $(wildcard *.hs)
BINS        := $(EXAMPLES:.hs=)
CLEAN_FILES += $(addsuffix $(exeext), $(BINS))

HC           = $(GHC_INPLACE)
SRC_HC_OPTS += -package GLUT

all:: $(BINS)

$(BINS): %$(exeext): %.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(HS_OBJS)

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
