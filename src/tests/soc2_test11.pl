:- use_module(helpers).

test :- 
    test_blocks_align,
    state_empty(S),
    add_DRAM(S,[],_),
    writeln("===== soc2-test11 listing ====="),
    decoding_net_listing,    
    writeln("===============================").
