.PHONY: clean

Main: Main.hs Interpreter.hs Parser.hs Types.hs
	ghc --make -dynamic -o $@ $<

clean:
	rm -rf Main *.hi *.o
