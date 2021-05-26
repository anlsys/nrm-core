.PHONY: all clean
all:
	cabal v2-build all

clean:
	cabal v2-clean 
