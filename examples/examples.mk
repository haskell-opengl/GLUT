ALL_DIRS         := $(SUBDIRS:%=all-%)
CLEAN_DIRS       := $(SUBDIRS:%=clean-%)
HC               := ghc
HC_FLAGS         := -v0 -Wall
HC_PATH_FLAGS    := $(HC_SEARCH_PATHS:%=-i%)
HS_SOURCES       := $(wildcard *.hs)
HS_PROG_SOURCES  := $(filter-out $(EXCLUDED_SOURCES), $(HS_SOURCES))
HS_PROGS         := $(HS_PROG_SOURCES:.hs=)

.PHONY: all $(ALL_DIRS) all-common clean $(CLEAN_DIRS) foo

all: $(ALL_DIRS) $(HS_PROGS)

$(ALL_DIRS): all-common
	$(MAKE) -C $(@:all-%=%) all

all-common:
ifneq ($(wildcard common),)
	$(MAKE) -C common all
endif

clean: $(CLEAN_DIRS)
ifneq ($(wildcard common),)
	$(MAKE) -C common clean
endif
ifneq ($(HS_SOURCES),)
	$(RM) $(HS_SOURCES:.hs=.hi) $(HS_SOURCES:.hs=.o) $(HS_PROG_SOURCES:.hs=.exe) $(HS_PROGS) $(wildcard *~)
endif

$(CLEAN_DIRS): 
	$(MAKE) -C $(@:clean-%=%) clean

$(HS_PROGS): %: %.hs
	$(HC) $(HC_FLAGS) $(HC_PATH_FLAGS) $(EXTRA_HC_FLAGS) $<

%.o: %.hs
	$(HC) $(HC_FLAGS) $(HC_PATH_FLAGS) $(EXTRA_HC_FLAGS) $<
