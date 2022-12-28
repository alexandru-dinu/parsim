PROJECT_NAME=monadic-parser
INPUT=

.PHONY: build run hindent

build:
	stack build

run:
	stack run $(PROJECT_NAME)-exe $(INPUT)

hindent:
	find . -name "*.hs" -exec $@ {} \;

hlint:
	$@ ./
