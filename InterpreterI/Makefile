# Simple SML Makefile

all: runall

verify: interpreter.sml
	echo 'use "interpreter.sml";' | sml

test: interpreter.sml test.sml
	echo 'use "test.sml";' | sml

runall: parser.sml
	./runall interpreter.sml

handin:
	handin akeen hw4 README.txt *.sml && handin akeen hw3


