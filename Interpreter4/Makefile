# Simple SML Makefile

all: runall

verify: interpreter.sml
	echo 'use "interpreter.sml";' | sml

test: interpreter.sml test.sml
	echo 'use "test.sml";' | sml

runall: interpreter.sml
	./runall interpreter.sml

out: interpreter.sml
	./runall interpreter.sml > output.diff

handin:
	handin akeen hw6 README.txt *.sml && handin akeen hw6

# Experiments with clojures
clo: closure.sml
	echo 'use "closure.sml";' | sml

