:- lib(ic).
:- lib(listut).
:- ["decoding_net5"].

:- dynamic translate/2.
:- dynamic accept/1.
:- dynamic overlay/2.
:- dynamic node_id_next/1.
:- dynamic node_id_node_enum/2.
:- dynamic module_tag/3.


blocks_same_length(block(B1,L1), block(B2,L2)) :-
    S1 is L1-B1,
    S2 is L2-B2,
    S1 = S2.

% blocks_align takes two list of blocks. If those two list add up to cover
% the same length, then Out1 and Out2 will cotain a list of blocks such that
% Out1 describes the same range as In1
% Out2 describes the same range as In2
% Out1has the same array-length as Out2
% Out1[x] has the same length Out2[x]
% Examples
% * blocks_align([block(1,10)], [block(101,110)], [block(1,10)], [block(101,110)])
% * blocks_align([block(1,5), block(6,10)], [block(101,110)],
%                [block(1,5), block(6,10)], [block(101, 105), block(106, 110)]).
blocks_align([], [], [], []).
blocks_align([In1B|In1Bs], [In2B|In2Bs], [In1B|Out1Bs], [In2B|Out2Bs]) :-
    % Case1: In1B == In2B
    blocks_same_length(In1B, In2B),
    blocks_align(In1Bs, In2Bs, Out1Bs, Out2Bs).

blocks_align([In1B|In1Bs], [In2B|In2Bs], [In1B|Out1Bs], [Out2B|Out2Bs]) :-
    % Case2: In1B smaller than In2B
    In1B = block(In1Base,In1Limit),
    In2B = block(In2Base,In2Limit),
    In1Size is In1Limit - In1Base + 1,
    In2Size is In2Limit - In2Base + 1,
    In1Size < In2Size,
    Out2Limit is In2Base + In1Size - 1,
    Out2B = block(In2Base, Out2Limit),

    In2RemBase is Out2Limit + 1,
    In2BRem = block(In2RemBase, In2Limit),

    blocks_align(In1Bs, [In2BRem | In2Bs], Out1Bs, Out2Bs).

blocks_align([In1B|In1Bs], [In2B|In2Bs], [Out1B|Out1Bs], [In2B|Out2Bs]) :-
    % Case3: In1B bigger than In2B
    In1B = block(In1Base,In1Limit),
    In2B = block(In2Base,In2Limit),
    In1Size is In1Limit - In1Base + 1,
    In2Size is In2Limit - In2Base + 1,
    In1Size > In2Size,

    Out1Limit is In1Base + In2Size - 1,
    Out1B = block(In1Base, Out1Limit),

    In1RemBase is Out1Limit + 1,
    In1BRem = block(In1RemBase, In1Limit),

    blocks_align([In1BRem | In1Bs], In2Bs, Out1Bs, Out2Bs).

test_blocks_align :-
    blocks_align([block(1,10)], [block(101,110)], [block(1,10)], [block(101,110)]),
    blocks_align([block(1,5), block(6,10)], [block(101,110)],  [block(1, 5), block(6, 10)], [block(101, 105), block(106, 110)]),
    blocks_align([block(101,110)], [block(1,5), block(6,10)],
                 [block(101, 105), block(106, 110)], [block(1, 5), block(6, 10)]).

% Turn an array-of-array-of-blocks into a pairwise aligned
% array-of-array-of-blocks
multi_blocks_align([], [], [], []).
multi_blocks_align([A | As], [B| Bs], [C | Cs], [D | Ds]) :-
    blocks_align(A,B,C,D),
    multi_blocks_align(As,Bs,Cs,Ds).

% For an array-of-arrays, generate 
multi_index_valid([], []).
multi_index_valid([A | As], [Idx | Idxs]) :-
	length(A, Limit),
	Idx :: [1 .. Limit],
	multi_index_valid(As, Idxs).

multi_indices(Mat, Indices) :-
    multi_index_valid(Mat, Index),
    findall(Index, labeling(Index), Indices).

multi_subscript([], [], []).
multi_subscript([M | Ms], [I | Is], [V | Vs]) :-
    array_list(MArr, M),
    V is MArr[I],
    multi_subscript(Ms,Is,Vs).
    
:- export assert_translate/4.
assert_translate(S,region(_, [[]],_),region(_, [[]], _),S).
assert_translate(S,region(InId, InBlocks,InProp),region(OutId, OutBlocks, OutProp),S) :-
    node_enum(InId, _),
    node_enum(OutId, _),
    %printf("at1  InBlocks=%p, OutBlocks=%p\n", [InBlocks, OutBlocks]),
    multi_blocks_align(InBlocks, OutBlocks, InAlBs, OutAlBs),
    %printf("at2  InAlBs=%p, OutAlBs=%p\n", [InAlBs, OutAlBs]),

    % InAlBs and OutAlBs are aligned, hence both should have the same structure.
    multi_indices(InAlBs, Indices),
    multi_indices(OutAlBs, Indices), 
    (foreach(Idx, Indices), param(InAlBs, OutAlBs, InId, InProp, OutId, OutProp)
        do
            multi_subscript(InAlBs, Idx, InB),
            multi_subscript(OutAlBs, Idx, OutB),
            %printf("Idx=%p, InB=%p, OutB=%p\n", [Idx, InB, OutB]),
            assert(translate(region(InId, InB, InProp), region(OutId, OutB, OutProp)))
    ).

:- export assert_overlay/4.
assert_overlay(S,A,B,S) :-
    node_enum(A, _),
    node_enum(B, _),
    assert(overlay(A,B)).

:- export assert_accept/3.
assert_accept(S,region(Id, Bs, Prop),S) :-
    node_enum(Id, _),
    (foreach(B, Bs), param(Id, Prop)
        do
            assert(accept(region(Id,B,Prop)))
    ).

:- export assert_module_tag/3.
assert_module_tag(Id,Name,Value) :-
    assert(module_tag(Id,Name,Value)).

:- export node_id_str/2.

:- export state_empty/1.
state_empty(state([],[],[])).

init :-
    assert(current_state(null)),
    assert(node_id_next(1)).

decoding_net_load_module(Module) :-
	init,
	state_empty(S),
	append_strings("add_",Module, CTargetS),
	atom_string(CTarget, CTargetS),
	call(CTarget, S, [], _), 
	assert(decoding_net_loaded).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% multid and nat values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multid_values_seq([block(Min,Max) | _], V) :-
    V :: [Min .. Max],
    labeling([V]).
multid_values_seq([_ | Blocks], V) :-
    multid_values_seq(Blocks, V).

multid_values_dim([], []).
multid_values_dim([Bs | BBs], [V | Vs]) :-
    multid_values_seq(Bs, V),
    multid_values_dim(BBs, Vs).

multid_values(Bs, Vs) :-
    findall(VsC, multid_values_dim(Bs, VsC), Vs).

nat_values(Bs, Vs) :-
    findall(VsC, multid_values_seq(Bs, VsC), Vs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Flatten
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% Replaces all multi-step translate with one step translates at NId.
%flatten(NId) :-
%    I = region(NId,_,_),
%    findall(m(I,O), decodes_to_accept(I, O, _), M),
%    printf("M=%p\n", [M]),
%    DelRegion = region(NId, _,_),
%    retractall(translate(DelRegion, _)),
%    (foreach(m(I,O),M) do
%        O = region(OId, block(OBase, _), Prop),
%        ON = name(OId, OBase, Prop),
%        assert(translate(I,ON))
%    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Node IDs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unused_node_enum(Enum) :-
    node_id_next(E),
    retractall(node_id_next(_)),
    Enum is E + 1,
    assert(node_id_next(Enum)).
node_enum(NodeId, Enum) :-
    node_id_node_enum(NodeId, Enum).
node_enum(NodeId, Enum) :-
    unused_node_enum(Enum),
    assert(node_id_node_enum(NodeId, Enum)).

node_find_fuzzy(Name,Out) :-
    node_id_node_enum(Out, _),
    member(L, Out),
    substring(L, _, _, Name).

node_str(Node,Str) :-
    nonvar(Node),
    reverse(Node, NodeR),
    join_string(NodeR,".",Str).
node_str(Node,Str) :-
    nonvar(Str),
    split_string(Str,".","",NodeR),
    reverse(NodeR, Node).

assert_node_exists(Node) :-
    ( node_id_node_enum(Node, _)
      ; 
      (sprintf(Err, "Error Node %p not found!",[Node]), throw(Err))
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Bit slice and concat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bitslice(InE, LoE, HiE, Out) :-
    % These evals make the predicate behave properly in "is" context.
    eval(InE, In), eval(LoE, Lo), eval(HiE, Hi),
    (
        for(InPos,Lo,Hi), param(Lo, In), fromto(0, OutI,OutO, Out)
    do
        Bit is getbit(In, InPos),
        (Bit = 1 -> OutO is setbit(OutI, InPos - Lo) ; OutO = OutI)
    ).

% Big endian, bitwise concat, A is upper order bits
% Out = (A << BBits) | B
bitconcat(AE, _, BE, BBitsE, Out) :-
    % These evals make the predicate behave properly in "is" context.
    eval(AE, A), eval(BE, B), eval(BBitsE, BBits),
    Out is (A << BBits) \/ B.
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Debug
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listing_term(S) :- write("    "), write(S), writeln(",").

:- export decoding_net_listing/0.
decoding_net_listing :-
    findall(t(A, B), ( node_id_node_enum(A,B), listing_term(node_id_node_enum(A,B) )), _),
    findall(t(A, B), ( translate(A,B), listing_term(translate(A,B) )), _),
    findall(t(A, B), ( overlay(A,B), listing_term(overlay(A,B) )), _),
    findall(t(A, B), ( accept(A), listing_term(accept(A) )), _).

