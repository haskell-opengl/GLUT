# -----------------------------------------------------------------------------

TOP = ../../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

EXAMPLES    := $(filter-out ReadImage.hs,$(wildcard *.hs))
BINS        := $(addsuffix $(exeext),$(EXAMPLES:.hs=))
CLEAN_FILES += $(BINS)

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -Wall -O -package GLUT

all:: $(BINS)

USES_READIMAGE = ColorTable Convolution ColorMatrix Histogram Minmax

.PRECIOUS: ReadImage.o
$(addsuffix .o,$(USES_READIMAGE)): ReadImage.hi
$(addsuffix $(exeext),$(USES_READIMAGE)): ReadImage.o

$(BINS): %$(exeext): %.o
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $< $(patsubst %,ReadImage.o,$(filter $(<:.o=),$(USES_READIMAGE)))

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
