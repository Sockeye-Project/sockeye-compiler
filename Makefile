########################################################################
## Copyright (c) 2017, ETH Zurich.
## All rights reserved.
##
## This file is distributed under the terms in the attached LICENSE file.
## If you do not find this file, copies can be found by writing to:
## ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
## Attn: Systems Group.
##
## Makefile for the Scokeye compiler
##
########################################################################

GHC=ghc

.PHONY: sockeye sockeye1 clean

bin/sockeye sockeye: src/*.hs
	mkdir -p build && mkdir -p bin
	${GHC} -o bin/sockeye -hidir build -odir build -isrc --make src/Main.hs

sockeye1:
	mkdir -p build/v1 && mkdir -p bin
	${GHC} -o bin/sockeye1 -hidir build/v1 -odir build/v1 -isrc/v1 --make src/v1/Main.hs

clean:
	rm -rf build
	rm -rf bin


build/tests/%.pl : src/tests/%.soc bin/sockeye
	mkdir -p build/tests
	./bin/sockeye -P $< -o $@

build/tests/%.txt : build/tests/%.pl src/tests/%.pl src-pl/*.pl
	eclipseclp -f src-pl/decoding_net5.pl \
			   -f src-pl/decoding_net5_support.pl \
			   -f src-pl/test-helpers.pl \
			   -f $(word 2,$^)\
			   -f $< \
			   -e "run_test(test, \"TMP.TXT\")"
	mv TMP.TXT $@

	
# TODO: Pattern match and run tests
run-tests: build/tests/soc2_test1.txt
	echo "Result of soc2_test1 is: "
	cat build/tests/soc2_test1.txt
