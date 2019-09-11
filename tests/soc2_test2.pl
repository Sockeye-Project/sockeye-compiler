:- use_module(helpers).

test :-
    init,
    state_empty(S),
    add_DRAM(S,[],_),
    overlay(["SOCKET_IN"],["SOCKET"]).

