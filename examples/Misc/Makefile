# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

EXAMPLES    := $(wildcard *.hs)
BINS        := $(EXAMPLES:.hs=)
CLEAN_FILES += $(addsuffix $(exeext), $(BINS))

HC           = $(GHC_INPLACE)
SRC_HC_OPTS += -Wall -O -package GLUT

all:: $(BINS)

$(BINS): %$(exeext): %.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
