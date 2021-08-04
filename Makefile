########################################################################
## Copyright (c) 2020, ETH Zurich.
## All rights reserved.
##
## This file is distributed under the terms in the attached LICENSE file.
## If you do not find this file, copies can be found by writing to:
## ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
## Attn: Systems Group.
##
## Makefile for the Sockeye compiler
##
########################################################################

GHC=ghc
ECLIPSE=eclipseclp
ALL_TEST_OUTPUTS = $(patsubst socs/tests/%.soc,build/socs/tests/%.txt,$(wildcard socs/tests/*.soc))

.PHONY: sockeye sockeye1 clean test_colibri

bin/sockeye sockeye: src/*.hs
	mkdir -p build && mkdir -p bin
	${GHC} -o bin/sockeye -hidir build -odir build -isrc --make src/Main.hs

sockeye1:
	mkdir -p build/v1 && mkdir -p bin
	${GHC} -o bin/sockeye1 -hidir build/v1 -odir build/v1 -isrc/v1 --make src/v1/Main.hs

clean:
	rm -rf build
	rm -rf bin


build/socs/%.pl : socs/%.soc bin/sockeye
	mkdir -p build/socs/tests
	./bin/sockeye -P $< -o $@

build/socs/tests/%.txt : build/socs/tests/%.pl socs/tests/%.pl src-pl/*.pl
	${ECLIPSE} -f src-pl/decoding_net5.pl \
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

####
#
#  Sample query for performance 
#
####

test_perf : src-pl/*.pl build/socs/x86_64_pc2.pl
	${ECLIPSE} -f src-pl/decoding_net5.pl \
			   -f src-pl/decoding_net5_support.pl \
			   -f src-pl/test-helpers.pl \
			   -f build/socs/x86_64_pc2.pl \
			   -f src-pl/test-perf.pl \
			   -e "run_test."

####
#
#  Colibri Test
#
####

test_colibri : build/socs/colibri.pl
	${ECLIPSE} -f src-pl/decoding_net5.pl \
			   -f src-pl/decoding_net5_support.pl \
			   -f src-pl/test-helpers.pl \
			   -f build/socs/colibri.pl \
			   -f src-pl/test-colibri.pl \
			   -e "run_test."

####
#
#  Rules for the page table generator
#
####

build/pt/cpu_ARMv8_FVP_Minimal_ARMCortexA57x1_Cluster0_ptable.c: build/socs/ARMv8_FVP_Minimal.pl
	mkdir -p build/pt
	${ECLIPSE} -f src-pl/decoding_net5.pl \
			   -f src-pl/decoding_net5_support.pl \
			   -f src-pl/test-helpers.pl \
			   -f build/socs/ARMv8_FVP_Minimal.pl \
			   -f src-ptgen/target/armv8/page_table_generator.pl \
			   -f src-ptgen/generate_page_table.pl \
			   -e "gen_pt(\"ARMv8_FVP_Minimal\", \"ARMCortexA57x1_Cluster0.CPUDRIVER\" , \"src-ptgen/target/armv8/page_table.c.in \" , \"build/pt/cpu_ARMv8_FVP_Minimal_ARMCortexA57x1_Cluster0_ptable.c \", 0, \"t0sz=16\")."

build/pt/cpu_knc_e225_ptable.c: build/socs/knc_e225.pl
	mkdir -p build/pt
	${ECLIPSE} -f src-pl/decoding_net5.pl \
			   -f src-pl/decoding_net5_support.pl \
			   -f src-pl/test-helpers.pl \
			   -f build/socs/knc_e225.pl \
			   -f src-ptgen/target/k1om/page_table_generator.pl \
			   -f src-ptgen/generate_page_table.pl \
			   -e "gen_pt_v1(\"PageTable_x86_XeonPhi\", \"BOOT\", \"src-ptgen/target/k1om/page_table.c.in \" , \"build/pt/cpu_knc_e225_ptable.c \", 0, \"skipEfi=true\")."

#test_pt: build/pt/cpu_ARMv8_FVP_Minimal_ARMCortexA57x1_Cluster0_ptable.c build/pt/cpu_knc_e225_ptable.c
test_pt: build/pt/cpu_knc_e225_ptable.c
	@echo "TODO: test what's in the C file 8-)"

diff_pt: build/pt/cpu_knc_e225_ptable.c ORIG.c
	meld build/pt/cpu_knc_e225_ptable.c ORIG.c

####
#
#  Rules for the bootinfo generation
#
####

build/bootinfo/boot_ARMv8_FVP_Minimal_ARMCortexA57x1_Cluster0_boot_consts.c: build/socs/ARMv8_FVP_Minimal.pl
	mkdir -p build/bootinfo
	${ECLIPSE} -f src-pl/decoding_net5.pl \
			   -f src-pl/decoding_net5_support.pl \
			   -f src-pl/test-helpers.pl \
			   -f build/socs/ARMv8_FVP_Minimal.pl \
			   -f src-ptgen/target/armv8/page_table_generator.pl \
			   -f src-ptgen/generate_page_table.pl \
			   -e "gen_bootinfo(\"ARMv8_FVP_Minimal\", \"ARMCortexA57x1_Cluster0.BOOT\" , \"build/bootinfo/boot_ARMv8_FVP_Minimal_ARMCortexA57x1_Cluster0_boot_consts.c \" )."

	

test_bootinfo: build/bootinfo/boot_ARMv8_FVP_Minimal_ARMCortexA57x1_Cluster0_boot_consts.c
	@echo "TODO: Test the output file: $<"

plos_xmpl: build/socs/plos-xmpl1.pl
	@echo "Example for SOSP compiled to $<"	
