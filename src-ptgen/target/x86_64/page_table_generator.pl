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

:- use_module(library(ic_global)).
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

%% we map it using two megabyte pages
sectionSize(SIZE)    :- SIZE   is 16'0000200000.
sectionSizeL3(SIZE)  :- SIZE   is 16'0040000000.
sectionSizeL4(SIZE)  :- SIZE   is 16'8000000000.



%%%%%%%%%%%%%%%%%%%%%
%% Code generation %%
%%%%%%%%%%%%%%%%%%%%%
tableIndexPDIR(VA,Index) :-
    sprintf(Index,"        [% 3u]",[VA]).
tableIndexPDPT(VA,Index) :-
    sprintf(Index,"    [% 3u]",[VA]).
tableIndexPML4(VA,Index) :-
    sprintf(Index,"[X86_64_PML4_TABLE_INDEX(0x%16rULL)]",[VA]).


memoryEntry(PA,Entry) :-
    sprintf(Entry,"PTABLE_MEMORY_ENTRY(0x%16rULL)",[PA]).

ptEntryL3(Idx,VA,Entry) :-
    sprintf(Entry,"PDIR_ENTRY(&x86_64_pdir[% 3u][% 3u])",[Idx,VA]).

ptEntryL4(VA,Entry) :-
    sprintf(Entry,"PDIR_ENTRY(&x86_64_pdpt[% 3u])",[VA]).

deviceEntry(PA,Entry) :-
    sprintf(Entry,"PTABLE_DEVICE_ENTRY(0x%16rULL)",[PA]).

pageTableEntry(Index,Entry,TableEntry) :-
    sprintf(TableEntry,"%s = %s",[Index,Entry]).

memoryPageTableEntry(VA,PA,TableEntry) :-
    tableIndexPDIR(VA,Index),
    memoryEntry(PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

devicePageTableEntry(VA,PA,TableEntry) :-
    tableIndexPDIR(VA,Index),
    deviceEntry(PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

memoryPageTableEntryL4(VA,PA,TableEntry) :-
    tableIndexPML4(VA,Index),
    ptEntryL4(PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

memoryPageTableEntryL3(Idx,VA,PA,TableEntry) :-
    tableIndexPDPT(VA,Index),
    ptEntryL3(Idx,PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

memoryPageTableEntryL2(VA,PA,TableEntry) :-
    tableIndexPDIR(VA,Index),
    memoryEntry(PA,Entry),
    pageTableEntry(Index,Entry,TableEntry).

memoryPageTableEntryInvalidL2(VA,TableEntry) :-
    tableIndexPDIR(VA,Index),
    pageTableEntry(Index,"PTABLE_ENTRY_INVALID",TableEntry).

memoryPageTableEntryInvalidL3(VA,TableEntry) :-
    tableIndexPDPT(VA,Index),
    pageTableEntry(Index,"PDIR_ENTRY_INVALID",TableEntry).

memoryPageTableEntryInvalidL4(VA,TableEntry) :-
    tableIndexPML4(VA,Index),
    pageTableEntry(Index,"PDIR_ENTRY_INVALID",TableEntry).



concatEntries(Entries,String) :-
    (foreach(EntryList,Entries), fromto("",Prev,Next,String) do
        ( is_list(EntryList) ->
            (foreach(Entry, EntryList), fromto("",Prev2,Next2,String2) do
                ( is_list(Entry) ->
                     join_string(Entry,",\n    ",ListString),
                     concat_strings(Prev2,ListString,Next2)
                ;
                    concat_strings(Prev2,Entry,Next2)
                )
            ),
            concat_strings(Prev,String2,Next)
            %Next = Prev
        ;
            %concat_strings(EntryList,",",EntryList2),
            concat_strings(Prev,EntryList,Next)
        )
    ).




concatMappings(Mappings,String) :-
    join_string(Mappings,",\n        ",String).


assert_align(Val,Align) :-
    mod(Val, Align, Res),
    (Res = 0 -> true;
    exitError("Memory region does not start at section boundry. 0x%016r, align=0x%016r, %u",
              [Val,Align,Res])).

%%%%%%%%%%%%%
%% Entries %%
%%%%%%%%%%%%%




ptEntriesL4(MemoryRegions,Offsets,Entries,NumEntries) :-
    % size a PML4 slot covers
    PML4SlotSize is 16'8000000000,
    PageTableSize is 512,

    % get a list of slots used for the memory regions
    (foreach(MemoryRegion, MemoryRegions),
     fromto([], Prev, Next, EntriesSet), param(PML4SlotSize) do
        pt_memory(BaseVA, Size) = MemoryRegion,
        div(BaseVA, PML4SlotSize, PFN),
        RoundedBaseVA is PFN * PML4SlotSize,
        EndVA is BaseVA + Size - 1,
        (for(VA,RoundedBaseVA,EndVA,sectionSizeL4),
         fromto([],Prev,Next,Entries2), param(PML4SlotSize) do
            div(VA, PML4SlotSize, RoundedBaseVA),
            mod(RoundedBaseVA, PML4SlotSize, StartEntrySlot),
            Next = [StartEntrySlot|Prev]
        ),
        union(Prev,Entries2,Next)
    ),

    % store the number of entries
    NumEntries is length(EntriesSet),

    % create an array
    dim(PML4Table,[PageTableSize]),

    % create the entries at the offsets
    (foreach(Offset,Offsets) * foreach(Entry,EntriesSet),
     param(PML4SlotSize), param(PML4Table), param(PageTableSize) do
        div(Offset, PML4SlotSize, PFNOffset),
        mod(PFNOffset, PageTableSize, PFNOffsetSlot),
        PFN is (PFNOffsetSlot + Entry + 1),
        arg(PFN,PML4Table,Entry)
    ),

    % fill unused slots with invalid entries
    (for(Slot,PageTableSize,1,-1), param(PML4SlotSize), param(PML4Table),
     fromto([],Prev,Next,Entries) do
        PFN is Slot - 1,
        VA is PFN * PML4SlotSize,
        arg(Slot,PML4Table,Qi),
        (nonvar(Qi) ->
            memoryPageTableEntryL4(VA,Qi,Entry),
            Next = [Entry| Prev]
        ;
            memoryPageTableEntryInvalidL4(VA,Entry),
            Next = [Entry| Prev]
        )
    ).


ptEntriesL3(MemoryRegions,_,Entries, NumTables, NumEntries) :-
    % size a PDPT slot covers
    PDPTSlotSize is 16'40000000,
    PageTableSize is 512,

    % get a list of slots used for the memory regions
    (foreach(MemoryRegion, MemoryRegions),
     fromto([], Prev, Next, EntriesSet), param(PDPTSlotSize) do
        pt_memory(BaseVA, Size) = MemoryRegion,
        div(BaseVA, PDPTSlotSize, PFN),
        RoundedBaseVA is PFN * PDPTSlotSize,
        EndVA is BaseVA + Size - 1,
        (for(VA,RoundedBaseVA,EndVA,sectionSizeL3),
         fromto([],Prev,Next,Entries3), param(PDPTSlotSize) do
            div(VA, PDPTSlotSize, StartEntrySlot),
            Next = [StartEntrySlot|Prev]
        ),
        union(Prev,Entries3,Next)
    ),

    % store the number of entries
    NumEntries is length(EntriesSet),

    % create an array
    dim(PDPTable, [NumTables, PageTableSize]),

    % create the entries at the offsets
    (foreach(Entry,EntriesSet), param(PDPTable), param(PageTableSize) do
        PFN is (Entry + 1),
        div(Entry, PageTableSize, TableIDX),
        PDPTableIndex is TableIDX + 1,
        arg([PDPTableIndex, PFN],PDPTable,Entry)
    ),

    % fill unused slots with invalid entries
    (for(Idx,1,NumTables,1), param(PDPTable),param(PageTableSize),
     fromto([],Prev,Next,Entries) do
        IdxArray is Idx - 1,
        sprintf(Hdr, "[%u] = {\n    ", IdxArray),
        (for(Slot,1,PageTableSize,1), param(PDPTable), param(Idx),param(IdxArray),
            fromto([],Prev,Next,RowEntries) do
            PFN is Slot - 1,

            arg([Idx,Slot],PDPTable,Qi),
            (nonvar(Qi) ->
                memoryPageTableEntryL3(IdxArray,PFN,Qi,Entry),
                append(Prev,[Entry], Next)
            ;
                memoryPageTableEntryInvalidL3(PFN,Entry),
                append(Prev,[Entry], Next)
            )
        ),
        %append(Prev,RowEntries,PrevIntermediate),
        append(Prev, [Hdr, RowEntries, "\n    }"], Next)
     ).


device_mapping_entry(VA,PA,Label,Entry) :-
    sprintf(Entry, "    DEVICE_MAPPING_ENTRY(0x%16rULL, 0x%16rULL, %s),\n",
            [PA,VA,Label]).


ptEntriesL2(MemoryRegions,DeviceRegions,Offsets,_,MaxMem,NumPML4Entries,
            NumPDPTEntries,L2Entries,DeviceMappings) :-
    % size a PDPT slot covers
    PML4SlotSize is 16'8000000000,
    PDPTSlotSize is 16'40000000,
    PDIRSlotSize is 16'00200000,
    PageTableSize is 512,

    % create an array
    dim(PDIRTable, [NumPML4Entries, NumPDPTEntries, PageTableSize]),

    % get a list of slots used for the memory regions
    (foreach(MemoryRegion, MemoryRegions), param(PDIRTable), param(PageTableSize),
     param(PDIRSlotSize), param(PML4SlotSize), param(PDPTSlotSize) do
        pt_memory(Base, Size) = MemoryRegion,
        EndAddr is Base + Size - 1,

        (for(VA, Base, EndAddr, PDIRSlotSize), param(PDIRTable), param(PageTableSize),
         param(PDIRSlotSize), param(PML4SlotSize), param(PDPTSlotSize),
         param(PageTableSize) do
            div(VA, PML4SlotSize, Tmp1),
            PML4TableIndex is Tmp1 + 1,

            div(VA, PDPTSlotSize, Tmp2),
            mod(Tmp2, PageTableSize, Tmp3),
            PDPTableIndex is Tmp3 + 1,

            div(VA, PDIRSlotSize, Tmp4),
            mod(Tmp4, PageTableSize, Tmp5),
            PDIRTableIndex is Tmp5 + 1,

            EntryIdx is PDIRTableIndex - 1,
            memoryPageTableEntryL2(EntryIdx,VA,Entry),
            arg([PML4TableIndex, PDPTableIndex, PDIRTableIndex],PDIRTable,Entry)
        )
    ),

    DevMemStart is MaxMem + 1,
    (foreach(DeviceRegion, DeviceRegions), fromto(DevMemStart,BaseVA,NVA,_),
     fromto([],DevMap,NextDevMap,DeviceMaps),
     param(PDIRTable), param(PageTableSize), param(Offsets),
     param(PDIRSlotSize), param(PML4SlotSize), param(PDPTSlotSize) do
        pt_device(Base,Size,Label) = DeviceRegion,
        EndAddr is Base + Size - 1,
        EndVA is BaseVA + Size - 1,

        (foreach(Offset, Offsets),
         fromto([],PrevEntries,NextEntries,Entry),
         param(BaseVA), param(Base), param(Label) do
            VA is BaseVA + Offset,
            device_mapping_entry(VA,Base,Label,Entry),
            NextEntries = [Entry | PrevEntries]
        ),


        % sprintf(NextDevMap, "%s%s", DevMap, Entry),
        append(DevMap, Entry, NextDevMap),

        (for(PA, Base, EndAddr, PDIRSlotSize), for(VA, BaseVA,EndVA, PDIRSlotSize),
         param(PDIRTable), param(PageTableSize), param(PDIRSlotSize), param(NVA),
         param(PML4SlotSize), param(PDPTSlotSize), param(PageTableSize) do

            div(VA, PML4SlotSize, Tmp1),
            PML4TableIndex is Tmp1 + 1,

            div(VA, PDPTSlotSize, Tmp2),
            mod(Tmp2, PageTableSize, Tmp3),
            PDPTableIndex is Tmp3 + 1,

            div(VA, PDIRSlotSize, Tmp4),
            mod(Tmp4, PageTableSize, Tmp5),
            PDIRTableIndex is Tmp5 + 1,


            EntryIdx is PDIRTableIndex - 1,
            devicePageTableEntry(EntryIdx,PA,Entry),
            arg([PML4TableIndex, PDPTableIndex, PDIRTableIndex],PDIRTable,Entry),

            NVA is VA + PDIRSlotSize
        )
    ),

    join_string(DeviceMaps,"",DeviceMappings),

    (for(PML4Idx, 1, NumPML4Entries, 1), fromto([],Prev,Next,L2Entries),
     param(NumPDPTEntries), param(PageTableSize),param(PDIRTable) do
        IdxArray is PML4Idx - 1,
        sprintf(Hdr, "[%u] = {\n    ", IdxArray),

        (for(PDPTIdx, 1, NumPDPTEntries, 1), fromto([],Prev2,Next2,Entries2),
         param(PageTableSize), param(PML4Idx),param(PDIRTable) do
            IdxArray is PDPTIdx - 1,
            sprintf(Hdr, "    [%u] = {\n    ", IdxArray),
            (for(PDIRIdx, 1, PageTableSize, 1), fromto([],Prev3,Next3,Entries3),
             param(PML4Idx), param(PDPTIdx),param(PDIRTable) do

                arg([PML4Idx,PDPTIdx, PDIRIdx],PDIRTable,Entry),
                (nonvar(Entry) ->
                    append(Prev3,[Entry], Next3)
                ;
                    EntryIDx is  PDIRIdx - 1,
                    memoryPageTableEntryInvalidL2(EntryIDx,EntryInvalid),
                    append(Prev3,[EntryInvalid], Next3)
                )
            ),
            % append(Prev2,Entries3, Next2),
            append(Prev2, [Hdr, Entries3, ",\n        },\n"], Next2)
        ),
        % append(Prev,Entries2, Next)
        append(Prev, [Hdr, Entries2, "\n    }"], Next)
    ).


:- dynamic deviceSectionMapped/2.


debug_print_memory_region(Base,Size) :-
    EndAddr is Base+Size-1,
    sprintf(Msg, " [0x%16rULL..0x%16rULL]", [Base, EndAddr]),
    writeln(Msg).

align_memory_regions(MemoryRegionsUnalign,MemoryRegions) :-
    MapSize is 16'0000200000,
    writeln("Unaligned:"),
    (foreach(MemoryRegion,MemoryRegionsUnalign),
    fromto([],PrevM,NextM,MemoryRegions), param(MapSize) do
        pt_memory(Base, Size) = MemoryRegion,
        debug_print_memory_region(Base, Size),
        div(Base, MapSize, BasePages),
        BaseRounded is BasePages * MapSize,
        SizeTmp is Size + MapSize - 1,
        div(SizeTmp, MapSize, SizePages),
        SizeRounded is SizePages * MapSize,
        append(PrevM, [pt_memory(BaseRounded, SizeRounded)], NextM)
    ),
    writeln("Aligned: "),
    (foreach(MemoryRegion,MemoryRegions) do
        pt_memory(Base, Size) = MemoryRegion,
        debug_print_memory_region(Base, Size)
    ).




find_memory_range(MemoryRegions, MaxMem,MinMem) :-
    MapSize is 16'0000200000,
    (foreach(MemoryRegion,MemoryRegions),
     fromto([],PrevM,NextM,Mappings), param(MapSize) do
        pt_memory(Base, Size) = MemoryRegion,
        EndAddr is Base + Size -1,
        assert_align(Base, MapSize),
        assert_align(Size, MapSize),
        append(PrevM, [Base, EndAddr], NextM)
    ),
    maxlist(Mappings,MaxMem),
    minlist(Mappings,MinMem).


device_memory_size(MemoryRegions, MaxMem)  :-
    MapSize is 16'0000200000,
    (foreach(MemoryRegion,MemoryRegions),
     fromto([],PrevM,NextM,Mappings), param(MapSize) do
        pt_device(_,Size,_) = MemoryRegion,
        SizeTmp is Size + MapSize - 1,
        div(SizeTmp, MapSize, SizePages),
        SizeRounded is SizePages * MapSize,
        append(PrevM, [SizeRounded], NextM)
    ),
    sumlist(Mappings,MaxMem).


%%%%%%%%%%
%% Main %%
%%%%%%%%%%



generate_pt_code(Template,_,NumDevices,MemoryRegionsUnalign,DeviceRegions,Offsets,Code) :-
    align_memory_regions(MemoryRegionsUnalign,MemoryRegions),

    find_memory_range(MemoryRegions, MaxMem, MinMem),

    device_memory_size(DeviceRegions, DevMemSize),

    AllMemoryRegions = [pt_memory(MaxMem, DevMemSize) | MemoryRegions],

    % create l4 entries
    ptEntriesL4(AllMemoryRegions,Offsets,PageTableL4Entries,NumPML4Entries),
    join_string(PageTableL4Entries,",\n    ",PageTableL4),

    % create l3 entries
    ptEntriesL3(AllMemoryRegions,Offsets,PageTableL3Entries,NumPML4Entries, NumPDPTEntries),
    (foreach(EntryList,PageTableL3Entries), fromto("",Prev,Next,PageTableL3) do
        ( is_list(EntryList) ->
            join_string(EntryList,",\n    ",ListString),
            concat_strings(Prev,ListString,Next)
            % Next = [ListString,Prev]
        ;
          %  join_string(EntryList,",\n    ",ListString),
            concat_strings(Prev,EntryList,Next)
        )
    ),

    % create l2 entries
    ptEntriesL2(MemoryRegions,DeviceRegions,Offsets,MinMem,MaxMem,NumPML4Entries,
                NumPDPTEntries,L2Entries,DeviceMappings),
    concatEntries(L2Entries,PageTable),



    sprintf(Code,Template,[NumPML4Entries,NumPDPTEntries,PageTable,
                           NumPML4Entries,PageTableL3,PageTableL4,
                           NumDevices,DeviceMappings]).

    % write template
    %split_string(Template,"?","",[Part1,_,Part2,_,Part3,_,Part4,_,Part5,_,Part6]),
    %sprintf(Code,"%s%s%s%s%s%s%s%s%s",[Part1,PageTable,Part2,PageTableL3,Part3,PageTableL4,Part4,Part5,Part6]).
