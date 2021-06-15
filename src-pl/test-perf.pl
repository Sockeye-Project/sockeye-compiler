interesting_region(InR) :-
    OutR = region(OutRName, _, _),
    accept(OutR),
    writeln(accept(OutR)),
    %printf("OutRName: %p\n", [OutRName]),
    InR = region(["SYSBUS"], [block(InB, _)], _),
    decodes_rev(OutR, _, InR),
    printf("%p --> %p\n", [InR, OutR]).

interesting_address(Addr) :-
    interesting_region(R),
    % for now we pick the base address of each region, we probably
    % want to extend this to sampling
    R = region(["SYSBUS"], [block(Addr, _)], _).


run_test :-
    printf("===== Finding memory regions =====\n",[]),
    init,
    state_empty(S),
    add_SYSTEM(S,[],_),
    printf("===== Listing decoding net =====\n", []),
    decoding_net_listing,
    printf("\n===== Listing Done =====\n", []),
    findall(R, interesting_address(R), AddrList),
    printf("%p\n", [AddrList]).
    %printf("==================================\n",[]).


