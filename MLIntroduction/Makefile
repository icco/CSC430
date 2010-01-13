# Simple SML Makefile

all: test

verify: functions.sml
	echo 'use "functions.sml";' | sml

test: functions.sml test.sml
	echo 'use "test.sml";' | sml
