# Simple SML Makefile

all: runall

verify: parser.sml
	echo 'use "rec.sml";' | sml

test: parser.sml lexer.sml test.sml
	echo 'use "test.sml";' | sml

runall: parser.sml
	./runall parser.sml
