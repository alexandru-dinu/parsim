PROJECT_NAME=parsim
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

test-hs:
	cabal test all

test-py:
	pytest -vv --hypothesis-show-statistics tests/test.py

clean:
	stack $@ --full
	find . -name ".hypothesis" -print0 | xargs -0 rm -rf
	find . -name ".pytest_cache" -print0 | xargs -0 rm -rf
	find . -name "__pycache__" -print0 | xargs -0 rm -rf
