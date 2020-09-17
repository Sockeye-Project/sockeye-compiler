% Calculate the intersection of A and B and put it into I
% The choice of the properties of B is a arbitrary, but we currently rely
% on that behavior.
reg_intersection(A, B, I) :-
    A = region(N, [block(ABase, ALimit)],_),
    B = region(N, [block(BBase, BLimit)],BProp),
    % Case 4: B contained entirely in A.
    (((ABase =< BBase, BLimit =< ALimit) -> I = region(N, [block(BBase, BLimit)],BProp)) ;
    (
    % Case 1: A contained entirely in B.
    (((BBase =< ABase, ALimit =< BLimit) -> I = region(N, [block(ABase, ALimit)],BProp)) ;
    (
        % Case 2: B overlaps on the right of A. BBase in A.
        (ABase =< BBase, BBase =< ALimit, I = region(N, [block(BBase, ALimit)],BProp)) ;

        % Case 3: B overlaps on the left of A. BLimit in A
        (ABase =< BLimit, BLimit =< ALimit, I = region(N, [block(ABase,BLimit)],BProp))
    )))).

% C is A - B. 
% - If they do not concern the same node, or do not overlap, C is A.
% - If B shoots a hole in A, the predicate will not resatisfy for left and right
reg_subtract(A, B, A) :-
    A = region(AN, [block(ABase, ALimit)],_),
    B = region(BN, [block(BBase, BLimit)],_),
    % No overlap
    (not(AN=BN) ; ALimit =< BBase ;  BLimit =< ABase ).
reg_subtract(A, B, C) :-
    A = region(N, [block(ABase, ALimit)],Prop),
    B = region(N, [block(BBase, BLimit)],Prop),
    C = region(N, [block(CBase, CLimit)],Prop),
    ((
        % A overlaps B to the left 
        ABase < BBase,
        BBase < ALimit,
        CBase = ABase,
        CLimit is BBase - 1
    ) ; (
        % A overlaps B to the right 
        ABase < BLimit,
        BLimit < ALimit,
        CBase is BLimit + 1,
        CLimit = ALimit
    )).
    

reg_subtract_li(I, [], I).
reg_subtract_li(In, [R | RM], Out) :-
    reg_subtract(In, R, Tmp),
    reg_subtract_li(Tmp, RM, Out).


decode_step(S,S,D) :- 
    S = region(NIn, Bl,_),
    D = region(NOut,Bl,_),
    overlay(NIn, NOut).

decode_step(S,SS,D) :- 
    translate(Src,DestRegion),
    reg_intersection(Src, S, SS),

    % Calculate D
    Src = region(_, [block(SrcBase, _)],_),
    SS = region(_, [block(SSBase, SSLimit)],_),
    DestRegion = region(DId,[block(DBase,_)],DProp),
    OutBase is DBase + (SSBase - SrcBase),
    OutLimit is OutBase + (SSLimit - SSBase),
    D = region(DId,[block(OutBase,OutLimit)],DProp).


decode(X,X,X).
decode(S,SS,D) :-
    decode_step(S, SSt, N),
    decode(N,SN, D),
    % Calculate SS
    % need to shrink SSt with N->SN and assign it to SS
    SSt=region(Id,[block(SStBase,SStLimit)],SStProp),
    N=region(_,[block(NBase,NLimit)],_),
    SN=region(_,[block(SNBase,SNLimit)],_),
    SSBase is SStBase + (SNBase - NBase),
    SSLimit is SStLimit - (NLimit-SNLimit),
    SS=region(Id,[block(SSBase,SSLimit)],SStProp).

% Predicates behaving like the forward version, but backwards
overlay_rev(A,B) :- overlay(B,A).
translate_rev(A,B) :- translate(B,A).
%    region(In, [block(InBase,InLimit)],_),
%    region(Out, [block(OutBase,OutLimit)], OutProp) :-
%        translate(region(Out, [block(OutBase,OutLimit)],_), region(In, [block(InBase,_)],OutProp)),
%        InLimit is InBase + (OutLimit - OutBase).


decode_step_rev(S,S,D) :- 
    S = region(NIn, Bl,_),
    D = region(NOut,Bl,_),
    overlay_rev(NIn,NOut).

decode_step_rev(S,SS,D) :- 
    translate_rev(Src,DestRegion),
    reg_intersection(Src, S, SS),

    % Calculate D
    Src = region(_, [block(SrcBase, _)],_),
    SS = region(_, [block(SSBase, SSLimit)],_),
    DestRegion = region(DId,[block(DBase,_)],DProp),
    OutBase is DBase + (SSBase - SrcBase),
    OutLimit is OutBase + (SSLimit - SSBase),
    D = region(DId,[block(OutBase,OutLimit)],DProp).


decodes_rev(X,X,X).
decodes_rev(S,SS,D) :-
    decode_step_rev(S, SSt, N),
    decodes_rev(N,SN, D),
    % Calculate SS
    % need to shrink SSt with N->SN and assign it to SS
    SSt=region(Id,[block(SStBase,SStLimit)],SStProp),
    N=region(_,[block(NBase,NLimit)],_),
    SN=region(_,[block(SNBase,SNLimit)],_),
    SSBase is SStBase + (SNBase - NBase),
    SSLimit is SStLimit - (NLimit-SNLimit),
    SS=region(Id,[block(SSBase,SSLimit)],SStProp).

% InRegion decodes OutRegion, OutRegion accepts with AccPred
decodes_to_accept(InRegion, OutRegion, AccPred) :-
    OutRegionS = region(_,_,AccPred),
    accept(OutRegionS),
    decodes_rev(OutRegionS, OutRegion, InRegion).
