run: *.hs
	ghc   -fforce-recomp -O3 --make -Wall -auto-all -fno-warn-missing-signatures Main.hs -o run

clean:
	rm -f *.hi *.o run

all: run