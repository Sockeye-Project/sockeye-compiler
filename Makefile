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

sockeye:
	mkdir -p build && mkdir -p bin
	${GHC} -o bin/sockeye -hidir build -odir build -isrc --make src/Main.hs

sockeye1:
	mkdir -p build/v1 && mkdir -p bin
	${GHC} -o bin/sockeye1 -hidir build/v1 -odir build/v1 -isrc/v1 --make src/v1/Main.hs

clean:
	rm -rf build
	rm -rf bin
