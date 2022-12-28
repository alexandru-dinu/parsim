PROJECT_NAME=monadic-parser
INPUT=

build:
	stack build

run:
	stack run $(PROJECT_NAME)-exe $(INPUT)
