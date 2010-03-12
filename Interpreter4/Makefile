# Simple SML Makefile

all: runall

verify:
	echo 'use "interpreter.sml";' | sml

test: test.sml
	echo 'use "test.sml";' | sml

runall: interpreter.sml
	./runall interpreter.sml

out: interpreter.sml
	./runall interpreter.sml > output.diff

handin:
	handin akeen hw7 README.txt *.sml && handin akeen hw7

