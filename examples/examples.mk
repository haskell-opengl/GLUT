ALL_DIRS         := $(SUBDIRS:%=all-%)
CLEAN_DIRS       := $(SUBDIRS:%=clean-%)
HC               := ghc
HC_FLAGS         := -v0 -Wall
HS_SOURCES       := $(wildcard *.hs)
HS_PROG_SOURCES  := $(filter-out $(EXCLUDED_SOURCES), $(HS_SOURCES))
HS_PROGS         := $(HS_PROG_SOURCES:.hs=)

.PHONY: all $(ALL_DIRS) clean $(CLEAN_DIRS)

all: $(ALL_DIRS) $(HS_PROGS)

$(ALL_DIRS): 
	$(MAKE) -C $(@:all-%=%) all

clean: $(CLEAN_DIRS)
ifneq ($(HS_SOURCES),)
	$(RM) $(HS_SOURCES:.hs=.hi) $(HS_SOURCES:.hs=.o) $(HS_PROG_SOURCES:.hs=.exe) $(HS_PROGS)
endif

$(CLEAN_DIRS): 
	$(MAKE) -C $(@:clean-%=%) clean

$(HS_PROGS): %: %.hs
	$(HC) $(HC_FLAGS) $(EXTRA_HC_FLAGS) $<

%.o: %.hs
	$(HC) $(HC_FLAGS) $(EXTRA_HC_FLAGS) $<
