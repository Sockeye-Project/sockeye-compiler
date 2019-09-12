:- use_module(helpers).

test :- 
    init,
    state_empty(S),
    add_SOCKET(S, [], _),
    translate(
        region(["DRAMMAP", [2]], [block(0, 4276092927)], true),
        region(["RAMOUT", [2]], [block(0, 4276092927)], true)),
    overlay(["RAMOUT", [2]], ["LOCAL"]),
    translate(
        region(["LOCAL_SRC"], [block(16384, 20479)], true),
        region(["GDDR0", [2]], [block(0, 4095)], true)).
