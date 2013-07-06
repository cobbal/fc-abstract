all: fib

fib: fib.hs plugin
	ghc -o fib -fplugin FCAbstract.Plugin fib.hs

clean: clean-plugin
	rm -f fib *.hi *.o

clean-plugin:
	(cd ..; cabal clean)

plugin:
	(cd ..; cabal configure && cabal build && cabal install)

.PHONY: fib
