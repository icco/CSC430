#!/bin/csh -f
# a simple way to test everything

if (${1} == "") then
echo "must provide source files (recognizer and tokenizer)"; exit 1
endif

foreach i ( tests/input/* )
	echo ==== Part 1 -- $i ====
	./run_rec ${1} $i tests/1_recognizer
end

#foreach i ( tests/input/* )
#	echo ==== Part2 -- $i ====
#	./run_tok ${2} $i tests/2_tokenizer
#end

