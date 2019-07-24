:- use_module(helpers).

test :- 
    writeln("H1"),
    test_blocks_align,
    writeln("H2"),
    state_empty(S),
    writeln("H3"),
    add_DRAM(S,[],_),
    writeln("===== soc2-test11 listing ====="),
    decoding_net_listing,
    writeln("===============================").
