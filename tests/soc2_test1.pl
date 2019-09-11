:- use_module(helpers).

test :-
    init,
    state_empty(S),
    add_DRAM(S,[],_),
    accept(region(["GDDR0"], [block(0,15)],_)),
    accept(region(["GDDR0"], [block(100,200)],_)),
    accept(region(["GDDR0"], [block(300,301)],_)),
    accept(region(["GDDR0"], [block(301,302)],_)),
    accept(region(["GDDR0"], [block(4096,4351)],_)).
