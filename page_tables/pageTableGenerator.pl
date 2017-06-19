%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(pageTableGenerator).
:- export pageTable/3.

memoryOffset(OFFSET) :- OFFSET is 16'80000000.
deviceOffset(OFFSET) :- OFFSET is 16'ff000000.
sectionSize(SIZE)	 :- SIZE   is 16'00100000.

tableIndex(VA,Index) :- 
	sprintf(Index,"ARM_L1_OFFSET(0x%16R)",[VA]).

deviceEntry(PA,Entry) :-
	sprintf(Entry,"DEVICE_ENTRY(0x%16R)",[PA]).

memoryEntry(PA,Entry) :- 
	sprintf(Entry,"MEMORY_ENTRY(0x%16R)",[PA]).

pageTableEntry(Index,Entry,TableEntry) :-
	sprintf(TableEntry,"[%s] = %s",[Index,Entry]).

devicePageTableEntry(VA,PA,TableEntry) :-
	tableIndex(VA,Index),
	deviceEntry(PA,Entry),
	pageTableEntry(Index,Entry,TableEntry).

memoryPageTableEntry(VA,PA,TableEntry) :-
	tableIndex(VA,Index),
	memoryEntry(PA,Entry),
	pageTableEntry(Index,Entry,TableEntry).

deviceEntries(DeviceRegions,Entries) :-
	FirstDevice is deviceOffset - sectionSize,
	(
        foreach(DeviceRegion,DeviceRegions),
        fromto([],Prev,Next,Entries),
        fromto(FirstDevice,PVA,NVA,_)
    do
        (_,PA,_) = DeviceRegion,
        VA is PVA + (PA rem sectionSize),
        devicePageTableEntry(VA,PA,Entry),
        Next = [Entry|Prev],
        NVA is PVA - sectionSize 
    ).

memoryEntries(MemoryRegions,Entries) :-
	FirstRegion is memoryOffset,
	(
        foreach(MemoryRegion,MemoryRegions),
        fromto([],PrevE,NextE,Entries),
        fromto(FirstRegion,PVA,NVA,_)
    do
        (_,BasePA,Size) = MemoryRegion,
        NVA is PVA + Size,
        (
        	for(VA,PVA,NVA-1,sectionSize),
        	for(PA,BasePA,BasePA+Size-1,sectionSize),
        	fromto([],PrevRE,NextRE,RegionEntries)
        do
        	memoryPageTableEntry(VA,PA,Entry),
        	NextRE = [Entry|PrevRE]
        ),
        append(RegionEntries,PrevE,NextE)
    ).

pageTable(DeviceRegions,MemoryRegions,Table) :-
	memoryEntries(MemoryRegions,MemoryEntries),
	deviceEntries(DeviceRegions,DeviceEntries),
	append(MemoryEntries,DeviceEntries,Entries),
	join_string(Entries,",\\\n",Table).
