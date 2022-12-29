PROJECT_NAME=monadic-parser
INPUT=

.PHONY: build run hindent hlint test clean

build:
	stack $@

run:
	stack run $(PROJECT_NAME)-exe $(INPUT)

hindent:
	find . -name "*.hs" -exec $@ {} \;

hlint:
	$@ ./

test:
	stack $@

clean:
	stack $@
