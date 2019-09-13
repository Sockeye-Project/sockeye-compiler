:- use_module(helpers).

test :-
    init,
    state_empty(S),
    add_SYS(S,[],_),
    decoding_net_listing.
