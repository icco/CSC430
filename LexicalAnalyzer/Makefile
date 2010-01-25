# Simple SML Makefile

all: test

verify: rec.sml tok.sml
	echo 'use "rec.sml";' | sml
	echo 'use "tok.sml";' | sml

test: rec.sml tok.sml test.sml
	echo 'use "test.sml";' | sml

runall: rec.sml tok.sml
	./runall rec.sml tok.sml
