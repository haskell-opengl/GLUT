# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2003/03/07 18:50:37 panne Exp $

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
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
