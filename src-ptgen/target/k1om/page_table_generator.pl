%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2019, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(page_table_generator).
:- export generate_pt_code/5.
:- lib(listut).

% TODO import this predicate from _support.pl
node_str(Node,Str) :-
    nonvar(Node),
    reverse(Node, NodeR),
    join_string(NodeR,".",Str).
node_str(Node,Str) :-
    nonvar(Str),
    split_string(Str,".","",NodeR),
    reverse(NodeR, Node).

%%%%%%%%%%%%%%%
%% Constants %%
%%%%%%%%%%%%%%%

page_sizes([16'8000000000,16'0040000000,16'0000200000,16'1000]). %'

%%%%%%%%%%%
%% Utils %%
%%%%%%%%%%%

page_table_entries_count(PTEntries) :-
    page_sizes([L0Size, L1Size | _]),
    PTEntries is L0Size // L1Size.

page_size(X) :-
    page_sizes(Sizes),
    member(X, Sizes).

split_va_rec(_, [], []).
split_va_rec(VA, [LC | LR], [PS | PR]) :-
    LC is VA // PS,
    VARem is (VA - LC*PS),
    split_va_rec(VARem, LR, PR). 

split_va(VA, Idx) :-
    page_sizes(PageSizes),
    split_va_rec(VA, Idx, PageSizes).

aligned(In, Size) :-
    In is (In // Size) * Size.
round_up(In, Size, Out) :-
    Out is (((In - 1) // Size) + 1) * Size.
round_down(In, Size, Out) :-
    Out is (In // Size) * Size.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map to PT Entry Conversion %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Given a Mapping M that is a multiple of a page size, return a PTE
map_pt(M, pte(Idx,addr(PAddr),Prop)) :-
    page_sizes(AllPageSizes),
    append(PageSizes, _, AllPageSizes), % will backtrack over all prefixes
    last(PageSize, PageSizes),
    M = map(VAddr, PageSize, PAddr, _, Prop),
    aligned(VAddr, PageSize),
    split_va_rec(VAddr,Idx,PageSizes).

%%%%%%%%%%%%%%%%%%%%
%% Map Splitting  %%
%%%%%%%%%%%%%%%%%%%%

% FM is the first, page sized submapping in M. 
splitfirst_map(M, FM) :-
    M = map(VAddr, Size, PAddr, Label, Prop),
    page_size(PageSize), % This will iterate over all page sizes.
    aligned(VAddr, PageSize),
    Size >= PageSize,
    FM = map(VAddr, PageSize, PAddr, Label, Prop).

% split map turns a map into a list of maps, each being nat-aligned of size 
% s with s in page_sizes.
split_map(map(_,0,_,_,_), []).
split_map(M, [FM | Maps]) :-
    M = map(VAddr, Size, PAddr, Label, Prop),
    splitfirst_map(M, FM),

    % Calc Mt, the remainder of M
    FM = map(_, FSize, _, _, _),
    MtVAddr is VAddr + FSize,
    MtPAddr is PAddr + FSize,
    MtSize is Size - FSize,
    Mt = map(MtVAddr, MtSize, MtPAddr, Label, Prop),

    % Recurse
    split_map(Mt, Maps).

split_maps([], []).
split_maps([M | Ms], Splits) :-
    split_map(M, S1),
    split_maps(Ms, S2),
    append(S1, S2, Splits).

shift_map(
    Off,
    map(VAddrIn, Size, PAddr, Label, Prop),
    map(VAddrOut, Size, PAddr, Label, Prop)) :-
        (VAddrIn < Off ->
            (sprintf(Err, "ERROR VAddr=%p too small for Off=%p", [VAddrIn, Off]),
             throw(Err))
        ;
            VAddrOut is VAddrIn - Off
        ).



pprint(map(VAddr, Size, PAddr, Label, Prop)) :-
    EndAddr is VAddr+Size-1,
    node_str(Label,LabelStr),
    printf(" [0x%16rULL..0x%16rULL] -> 0x%16rULL     %p,%p\n",
        [VAddr, EndAddr, PAddr,LabelStr, Prop]).
pprint(pte(Idx, table(TIdx), _)) :-
    printf(" %p -> %p\n", [Idx, table(TIdx)]).
pprint(pte(Idx, addr(PAddr), Prop)) :-
    printf(" %p -> 0x%16rULL     %p\n",[Idx, PAddr, Prop]).

align_memory_regions([], []).
align_memory_regions([M | Ms], [A | As]) :-
    page_sizes(Sizes),
    last(PageSize,Sizes),
    M = map(VAddr, Size, PAddr, Label, Prop),
    round_down(VAddr, PageSize, VAddrRounded),
    round_down(PAddr, PageSize, PAddrRounded),
    round_up(Size, PageSize, SizeRounded),
    A = map(VAddrRounded, SizeRounded, PAddrRounded, Label, Prop),
    align_memory_regions(Ms,As).

%%%%%%%%%%%%%%%%%%%%%%%
%% C-Code Generation %%
%%%%%%%%%%%%%%%%%%%%%%%

% Prefix is a non-empty, non-trivial prefix of Li.
list_prefix(Li, Prefix) :-
    reverse(Li, [_ | LoRev]),
    reverse(LoRev,Prefix),
    length(Prefix, PrefixL),
    PrefixL > 0.
list_prefix(Li, Prefix) :-
    reverse(Li, [_ | LoRev]),
    reverse(LoRev,PrefixT),
    list_prefix(PrefixT, Prefix).
c_pt_to_alloc([pte(Idx,_,_) | _], PtIdx) :-
    list_prefix(Idx, PtIdx).
c_pt_to_alloc([_ | PTEs], PtIdx) :-
    c_pt_to_alloc(PTEs, PtIdx).

c_pt_entry_macro([_,_,_], addr(_),  devreg,  "ARMV8_L3_DEVICE_ENTRY").
c_pt_entry_macro([_,_,_], addr(_),  mem,     "ARMV8_L3_MEMORY_ENTRY").
c_pt_entry_macro([_,_,_], _      ,  invalid, "ARMV8_L3_ENTRY_INVALID").
c_pt_entry_macro([_,_],   addr(_),  devreg,  "ARMV8_L2_DEVICE_ENTRY").
c_pt_entry_macro([_,_],   addr(_),  mem,     "ARMV8_L2_MEMORY_ENTRY").
c_pt_entry_macro([_,_],   _      ,  invalid, "ARMV8_L2_ENTRY_INVALID").
c_pt_entry_macro([_,_],   table(_), table,   "ARMV8_L2_TABLE_ENTRY").
c_pt_entry_macro([_],     addr(_),  devreg,  "ARMV8_L1_DEVICE_ENTRY").
c_pt_entry_macro([_],     _,        invalid, "ARMV8_L1_ENTRY_INVALID").
c_pt_entry_macro([_],     addr(_),  mem,     "ARMV8_L1_MEMORY_ENTRY").
c_pt_entry_macro([_],     table(_), table,   "ARMV8_L1_TABLE_ENTRY").
c_pt_entry_macro([],      table(_), table,   "ARMV8_L0_TABLE_ENTRY").
c_pt_entry_macro([],      _,        invalid, "ARMV8_L0_ENTRY_INVALID").
% TODO: Make bootrom entries RO
c_pt_entry_macro(A,B,bootrom,D) :- c_pt_entry_macro(A,B,mem,D).
c_pt_entry_macro(A,B,C,D) :- 
    sprintf(Err, "ERROR c_pt_entry_macro(%p,%p,%p,%p) UNDEFINED\n", [A,B,C,D]),
    throw(Err).

c_pt_entry_invalid(Idx, Out) :- c_pt_entry_macro(Idx, _, invalid, Out).
c_pt_entry(Idx,table(TablePos),_, Out) :- 
    c_pt_entry_macro(Idx, table(TablePos), table, Macro),
    sprintf(Out, "%s(%d)", [Macro, TablePos]).
c_pt_entry(Idx,addr(Addr),Prop, Out) :- 
    c_pt_entry_macro(Idx, addr(Addr), Prop, Macro),
    sprintf(Out, "%s(0x%16rULL)", [Macro, Addr]).

% Turn a PT index ([0,1,2]) into a pte([0,1], table(table idx of [0,1,2]), true))
pt_pte(PTS, PT, pte(PT, table(TIdx), true)) :-
    nth0(TIdx, PTS, PT).

c_code_for_pt(PTEs, PT, Str) :-
    % - Filter PTEs with PT prefix. Idx = PT ++ [TablePos]
    (foreach(PTE,PTEs), fromto([],Prev,Next,PTEsFiltUnsorted),param(PT) do
        (PTE = pte(Idx, Addr, Prop),
        append(PT,[TablePos],Idx)) ->
            (Next = [pte(TablePos,Addr,Prop) | Prev])
        ;
            Next = Prev
    ),
    % - sort PTEs by TablePos 
    sort(PTEsFiltUnsorted,PTEsFilt),
    page_table_entries_count(NumPtEntries),
    Max is NumPtEntries - 1,
    (for(TablePos, 0, Max),param(PTEsFilt),param(PT),foreach(Out,PTEsStr) do 
        (nth0(_, PTEsFilt, pte(TablePos,Addr,Prop)) ->
           c_pt_entry(PT,Addr,Prop, OutR)
       ;
           c_pt_entry_invalid(PT, OutR)
       ),
       sprintf(Out, "        [%d] = %s",  [TablePos, OutR])
    ),

    % - Output
    join_string(PTEsStr, ",\n", TableContStr),
    length(PT, PTLevel),
    sprintf(Cmt, "//For PT=%p, Level=%d", [PT,PTLevel]), 
    sprintf(Cod, "{ .l%d = {\n%s \n}\n}", [PTLevel, TableContStr]),
    join_string([Cmt,Cod], "\n", Str).

c_code(MapPTEs, PTSStr, PTSNum) :-
    setof(PtIdx, c_pt_to_alloc(MapPTEs, PtIdx), PTST),
    % PTS is an array of all PT indices used, for instance [[0,1],[0],[0,0,224]] 
    % means pt_0_1, pt_0 pt_0_0_224 are used. Prefix this with [] to account
    % for the root page table (always at position 0).
    PTS = [ [] | PTST],

    % writeln("Using the following array of Page tables"),
    % writeln(PTS),

    maplist(pt_pte(PTS), PTST, PtPTEs),
    append(MapPTEs, PtPTEs, PTEs),
    maplist(c_code_for_pt(PTEs), PTS, PTStrs),
    join_string(PTStrs, ",\n", PTSStr),
    length(PTS, PTSNum).

%%%%%%%%%%%%%%%%%%%%%
%% Device Mappings %%
%%%%%%%%%%%%%%%%%%%%%
device_mapping_entry_fmt(VA,PA,Label,Entry) :-
    node_str(Label,LabelStr),
    sprintf(Entry, "    DEVICE_MAPPING_ENTRY(0x%16rULL, 0x%16rULL, %s),\n",
            [PA,VA,LabelStr]).

device_mapping_entry(Maps, Entry) :-
    member(map(BaseVA, _, BasePA, Label, _),Maps),
    device_mapping_entry_fmt(BaseVA, BasePA, Label, Entry).

device_mappings(Maps, Str, Num) :-
    findall(E, device_mapping_entry(Maps, E), DevMaps),
    length(DevMaps, Num),
    join_string(DevMaps,"",Str).

%%%%%%%%%%
%% Main %%
%%%%%%%%%%

%target_arg_term("aaa=asd,aax=3", aaa("asd")) --> True
target_arg_term(ArgStr,Term) :-
    split_string(ArgStr,",","",Sp),
    member(S,Sp),
    split_string(S,"=","",[As,B]),
    atom_string(A, As),
    (number_string(BNum, B) -> Term =.. [A,BNum] ; Term =.. [A,B]).

% Template - Template C code as string
% MapsUnalign - Mappings to be installed. A List of map(VAddr, Size, PAddr,
%     Label, Prop) terms. Prop can be mem or devreg, Label is used for list 
%     of regions.
generate_pt_code(Template, MapsUnalign, PageTableBase, TargetArg, Code) :-
    % Align to at least page size
    align_memory_regions(MapsUnalign,MapsAlign),

    % If we got a T0SZ argument, we generate a PT for a shift address space
    % hence we subtract 2**T0SZ from all VAddr
    (target_arg_term(TargetArg, t0sz(T0SZ)) ; T0SZ = 0), !,
    Off is (2^64 - 2^(64-T0SZ)),
    maplist(shift_map(Off), MapsAlign, MapsShift),

    % Split maps into multiples of (any) page size
    split_maps(MapsShift, MapsSplit),
    !, % Use only the first (ideal) split.

    % writeln("Aligned, Shifted & Split Mappings:"),
    % checklist(pprint, MapsSplit),

    maplist(map_pt, MapsSplit, PTEs),

    % writeln("PT Entries:"),
    % checklist(pprint, PTEs),

    c_code(PTEs, PTSStr, PTSNum),
    device_mappings(MapsUnalign, DMapStr, DMapNum),
    sprintf(Code, Template,
        [PTSNum, PageTableBase, PTSStr, DMapNum, DMapStr, "%s"]).

