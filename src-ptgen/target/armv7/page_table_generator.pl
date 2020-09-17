%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(page_table_generator).
:- export generate_pt_code/7.

:- use_module(helpers).

/**
 * Kernel virtual address space layout:
 *
 * 00000000-7FFFFFFFF: 1-1 mappings (hardware we have not mapped
 *                     into high kernel space yet, and the init
 *                     code that is currently executing, in case
 *                     RAM doesn't start at 80000000).
 * 80000000-BFFFFFFFF: 1-1 mappings (this is 1GB of RAM).
 * C0000000-FEFFFFFFF: Hardware devices,
 *                     allocated descending from deviceOffset.
 * FF000000-FFEFFFFFF: Unallocated.
 * FFF00000-FFFFFFFFF: L2 table, containing:
 *      FFF00000-FFFEFFFF: Unallocated
 *      FFFF0000-FFFFFFFF: Exception vectors
 *
 * 
 * It makes sense to use sections for the hardware devices since (1) we don't map many
 * devices in the CPU driver anyway, and (2) if we did, it might
 * save a wee bit of TLB space. 
 */

%%%%%%%%%%%%%%%
%% Constants %%
%%%%%%%%%%%%%%%
sectionSize(SIZE)    :- SIZE   is 16'00100000.

oneToOneStart(START) :- START is 16'00000000.
oneToOneSize(SIZE)   :- SIZE  is 16'80000000.

memoryStart(START) :- START is 16'80000000.
memorySize(SIZE)   :- SIZE  is 16'40000000.

devicesStart(START) :- START is 16'c0000000.
devicesSize(SIZE)   :- SIZE  is 16'3f000000.

%%%%%%%%%%%%%%%%%%%%%
%% Code generation %%
%%%%%%%%%%%%%%%%%%%%%
tableIndex(VA,Index) :- 
    sprintf(Index,"L1_TABLE_INDEX(0x%08lx)",[VA]).

memoryEntry(PA,Entry) :-
    sprintf(Entry,"L1_MEMORY_ENTRY(0x%08lx)",[PA]).

deviceEntry(PA,Entry) :-
    sprintf(Entry,"L1_DEVICE_ENTRY(0x%08lx)",[PA]).

pageTableEntry(Index,Entry,TableEntry) :-
    sprintf(TableEntry,"[%s] = %s",[Index,Entry]).

memoryPageTableEntry(VA,PA,TableEntry) :-
    tableIndex(VA,Index),
    memoryEntry(PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

devicePageTableEntry(VA,PA,TableEntry) :-
    tableIndex(VA,Index),
    deviceEntry(PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

deviceMappingEntry(VA,PA,Label,MappingEntry) :-
    sprintf(MappingEntry,"DEVICE_MAPPING_ENTRY(0x%08x, 0x%08x, %s),",[PA,VA,Label]).

concatEntries(Entries,String) :-
    (
        foreach(EntryList,Entries),
        fromto([],Prev,Next,EntryStrings)
    do
        ( EntryList = [] ->
            Next = Prev
        ;
            join_string(EntryList,",\n    ",ListString),
            Next = [ListString|Prev]
        )
    ),
    join_string(EntryStrings,",\n    ",String).

concatMappings(Mappings,String) :-
    join_string(Mappings,"\n        ",String).

%%%%%%%%%%%%%
%% Entries %%
%%%%%%%%%%%%%

%% 1-1
oneToOneEntries(Entries) :-
    LastA is oneToOneStart + oneToOneSize - 1,
    (
        for(A,oneToOneStart,LastA,sectionSize),
        fromto([],Prev,Next,Entries)
    do
        devicePageTableEntry(A,A,Entry),
        Next = [Entry|Prev]
    ).

%% Memory
checkSectionStart(Addr) :-
    (Addr mod sectionSize) =:= 0;
    exitError("Memory region does not start at section boundry.").

memoryEntries([MemoryRegion],Entries) :-
    pt_memory(BasePA,Size) = MemoryRegion,
    writeln(MemoryRegion),
    checkSectionStart(BasePA),
    MemorySize is min(Size,memorySize),
    LastVA is memoryStart + MemorySize - 1,
    LastPA is BasePA + MemorySize - 1,
    (
        for(VA,memoryStart,LastVA,sectionSize),
        for(PA,BasePA,LastPA,sectionSize),
        fromto([],Prev,Next,Entries)
    do
        memoryPageTableEntry(VA,PA,Entry),
        % Next = [Entry|Prev]
        append(Prev, [Entry], Next)
    ).

%% Devices
checkDeviceFitsSection(Size) :-
    Size =< sectionSize;
    exitError("Device spans more than one section.").

checkInsideDeviceSpace(Addr) :-
    Addr >= devicesStart,
    Addr < devicesStart + devicesSize;
    exitError("Trying to map device outside device region.").

:- dynamic deviceSectionMapped/2.
deviceEntries(DeviceRegions,Entries,Mappings) :-
    FirstDevice is devicesStart + devicesSize - sectionSize,
    (
        foreach(DeviceRegion,DeviceRegions),
        fromto([],Prev,Next,Entries),
        fromto(FirstDevice,BaseVA,NVA,_),
        fromto([],PrevM,NextM,Mappings)
    do
        pt_device(PA,Size,Label) = DeviceRegion,
        checkDeviceFitsSection(Size),
        Offset is (PA rem sectionSize),
        SecBase is PA - Offset,
        ( deviceSectionMapped(SecBase,_) ->
            Next = Prev,
            NVA is BaseVA
        ;
            checkInsideDeviceSpace(BaseVA),
            devicePageTableEntry(BaseVA,SecBase,Entry),
            assert(deviceSectionMapped(SecBase,BaseVA)),
            append(Prev,[Entry], Next),
            % Next = [Entry|Prev],
            NVA is BaseVA - sectionSize
        ),
        deviceSectionMapped(SecBase,MappedVA),
        VA is MappedVA + Offset,
        deviceMappingEntry(VA,PA,Label,MappingEntry),
        NextM = [MappingEntry|PrevM]
    ).

%%%%%%%%%%
%% Main %%
%%%%%%%%%%
generate_pt_code(Template,_,NumDevices,MemoryRegions,DeviceRegions,_,Code) :-
    writeln(MemoryRegions),

    oneToOneEntries(OneToOneEntries),
    writeln("A"),
    memoryEntries(MemoryRegions,MemoryEntries),
    writeln("B"),
    deviceEntries(DeviceRegions,DeviceEntries,Mappings),
    writeln("C"),
    concatEntries([DeviceEntries,MemoryEntries,OneToOneEntries],PageTable),
    writeln("D"),
    concatMappings(Mappings,DeviceMappings),

    sprintf(Code,Template,[PageTable,NumDevices,DeviceMappings]).


