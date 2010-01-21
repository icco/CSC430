# Simple SML Makefile

all: test

verify: rec.sml
	echo 'use "rec.sml";' | sml

test: rec.sml test.sml
	echo 'use "test.sml";' | sml
