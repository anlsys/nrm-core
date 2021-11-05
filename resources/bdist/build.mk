# Full build with max optimisation and everything enabled (very slow build)
BuildFlavour = perf

ifneq "$(BuildFlavour)" ""
include mk/flavours/$(BuildFlavour).mk
endif

GhcLibHcOpts += -fPIC 
GhcRtsHcOpts += -fPIC 

# Don't strip debug and other unneeded symbols from libraries and executables.
STRIP_CMD = :
