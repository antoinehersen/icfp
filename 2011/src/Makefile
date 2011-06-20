run: *.hs
	ghc   -fforce-recomp  -rtsopts -with-rtsopts="-hc" -O3 --make -Wall -prof -auto-all -fno-warn-missing-signatures Main.hs -o run

clean:
	rm -f *.hi *.o run

all: run