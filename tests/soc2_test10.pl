:- use_module(helpers).

test :- 
    init,
    state_empty(S),
    add_SOCKET(S, [], _),
    decoding_net_listing.
