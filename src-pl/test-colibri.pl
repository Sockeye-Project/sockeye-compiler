run_test :-
    printf("Testing colibri.pl\n",[]),
    init,
    state_empty(S),
    add_ROOT(S,[],_),
    printf("===== Listing decoding net =====\n", []),
    decoding_net_listing,
    printf("\n===== Listing Done =====\n", []),
    OutR = region(["LOCAL", "ROOT_USDHC_5B010000"], _, _),
    accept(OutR),
    InR = region(["LOCAL_domain"], [block(InB,InLimit)], _),
    decodes_rev(OutR, _, InR),
    printf("%p --> %p\n", [InR, OutR]),
    printf("USDHC Base Address: 0x%x\n", [InB]).
