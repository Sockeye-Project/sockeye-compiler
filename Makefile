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
ALL_TEST_OUTPUTS = $(patsubst src/tests/%.soc,build/tests/%.txt,$(wildcard src/tests/*.soc))

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

	
build/test_report.txt: $(ALL_TEST_OUTPUTS)
	echo "Test Report:" > build/test_report.txt
	echo "============" >> build/test_report.txt
	for t in $(ALL_TEST_OUTPUTS) ; do\
		echo $$t >> build/test_report.txt && \
		cat $$t >> build/test_report.txt && \
		echo "" >> build/test_report.txt; \
	done

test: build/test_report.txt
	@cat build/test_report.txt
