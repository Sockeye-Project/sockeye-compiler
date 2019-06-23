:- use_module(helpers).

test :-
    init,
    state_empty(S),
    add_SYS(S,[],_),
    % Check accept size
    accept(region(_, block(0,4598),_)),
    % Check module tag
    module_tag(["dram0"], "mpid", 16'100). %'
