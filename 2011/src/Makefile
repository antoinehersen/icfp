run: *.hs
	ghc -rtsopts  -with-rtsopts="-M900M" -O3 --make -Wall -fno-warn-missing-signatures Main.hs -o run

clean:
	rm -f *.hi *.o run

all: run