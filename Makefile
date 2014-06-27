GHC=ghc
HCFLAGS=--make -O -dynamic -optc -g

Main : Main.hs Execute.c Execute.h
	$(GHC) $(HCFLAGS) Main.hs Execute.c
