TARGETS=Main Test

all: $(TARGETS)

Main: Main.hs
	ghc -dynamic Main.hs

Test: Test.hs
	ghc -dynamic Test.hs

clean:
	rm -f $(TARGETS) *.hi *.o
