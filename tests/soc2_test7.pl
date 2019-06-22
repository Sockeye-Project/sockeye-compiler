:- use_module(helpers).

test :-
    init,
    state_empty(S),
    add_SYS(S,[],_),
    accept(region(_, block(0,4598),_)).
