# Simple SML Makefile

all: runall

verify: parser.sml
	echo 'use "parser.sml";' | sml

test: parser.sml lexer.sml test.sml
	echo 'use "test.sml";' | sml

part1: parser.sml
	./runPart1 parser.sml

runall: parser.sml
	./runall parser.sml

handin:
	handin akeen hw3 README.txt lexer.sml parser.sml && handin akeen hw3
