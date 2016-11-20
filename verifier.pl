:- ensure_loaded(library(lists)).

% Negation - required by grammar, defined the same as `=\=`.
:- op(700, xfx, <>).

verify(N, FileName) :-
    set_prolog_flag(fileerrors, off),
    N > 0,
    see(FileName),
    !,  % Allowed red cut.
    read(vars(Vars)),
    read(arrays(As)),
    read(program(Insts)),
    seen,
    initState(p(vars(Vars), arrays(As), program(Insts)), N, Init),
    verifyState(p(vars(Vars), arrays(As), program(Insts)), 1, [Init], [Init]).
verify(N, _FileName) :-
    N =< 0,
    write('Error: parameter 0 should ba a number > 0'),
    !.  % Allowed red cut.
verify(_N, FileName) :-
    format('Error: there is no file with given name - ~p~n', [FileName]).

% Verify loaded `Program` in given `State`.
verifyState(_Program, _, [], _Verified) :-
    write('Program is correct (thread-safe).').
verifyState(Program, StateNo, [State | ToVerify], KnownStates) :-
    programsInSection(Program, State, PIDs),
    length(PIDs, ProgramsInSection),
    (ProgramsInSection < 2 ->
        nextStates(Program, State, NextStates),
        distinct(NextStates, KnownStates, NewStates),
        append(NewStates, KnownStates, NewKnownStates),
        append(NewStates, ToVerify, NewToVerify),
        NextStateNo is StateNo + 1,
        verifyState(Program, NextStateNo, NewToVerify, NewKnownStates)
    ;
        format(
            'Program is incorrect: state no ~p is not thread-safe.~n',
            [StateNo]
        ),
        printHistory(State),
        format('Processes in critical section: ~p.~n', [PIDs])
    ).

% Prints interleaving calls.
printHistory(s(pcs(_PCs), values(_Vs), arrayValues(_AVs), log(H))) :-
    write('Incorrect sequence:'), nl,
    reverse(H, ChronologicalH),
    printHistoryAux(ChronologicalH).
printHistoryAux([]).
printHistoryAux([[PID, InstNo] | H]) :-
    format('    Process ~p: ~p~n', [PID, InstNo]),
    printHistoryAux(H).

% Generates next states from given one using auxiliary cluase with accumulator.
nextStates(
    Program,
    s(pcs(PCs), values(Vs), arrayValues(AVs), log(H)),
    NextStates
) :-
    nextStatesAcc(
        Program,
        s(pcs(PCs), values(Vs), arrayValues(AVs), log(H)),
        PCs,
        0,
        [],
        NextStates
    ).
nextStatesAcc(_Program, _State, [], _I, NextStates, NextStates).
nextStatesAcc(Program, State, [_PC | PCs], I, Acc, NextStates) :-
    step(Program, State, I, NextState),
    I1 is I + 1,
    nextStatesAcc(Program, State, PCs, I1, [NextState | Acc], NextStates).

% PIDs is a list containing process ids of program `p` which are in critical
% section for state `s`.
programsInSection(
    p(vars(_Vars), arrays(_As), program(Insts)),
    s(pcs(PCs), values(_Vs), arrayValues(_AVs), log(_H)),
    PIDs
) :-
    find1(section, Insts, Sections),
    findL0(Sections, PCs, PIDs).

% `Indices` is a list of indices of element `X` in list `L`
% (counting the first element as 1).
find1(X, L, Indices) :- findL(1, [X], L, Indices).

% `Indices` is a list of indices of elements from list `K` in list `L`
% (counting the first element as 0).
findL0(K, L, Indices) :- findL(0, K, L, Indices).

% `Indices` is a list of indices of elements from list `K` in list `L`
% (counting the first element as N).
findL(_N, _K, [], []).
findL(N, K, [X | L], [N | Indices]) :-
    member(X, K),
    N1 is N + 1,
    findL(N1, K, L, Indices).
findL(N, K, [X | L], Indices) :-
    \+ member(X, K),
    N1 is N + 1,
    findL(N1, K, L, Indices).

% `R` is a states list `L` without states from list `F`.
distinct([], _, []).
distinct([E | L], F, R) :-
    stateMember(E, F),
    distinct(L, F, R).
distinct([E | L], F, [E | R]) :-
    \+ stateMember(E, F),
    distinct(L, F, R).

% State `s` is in given list of states (omitting logs).
stateMember(
    s(pcs(PCs), values(Vs), arrayValues(AVs), log(_)),
    [s(pcs(PCs), values(Vs), arrayValues(AVs), log(_)) | _States]
).
stateMember(
    s(pcs(PCs), values(Vs), arrayValues(AVs), log(X)),
    [s(pcs(_PCs), values(_Vs), arrayValues(_AVs), log(_X)) | States]
) :- stateMember(s(pcs(PCs), values(Vs), arrayValues(AVs), log(X)), States).

% initState(+Program, +N, -InitialState)
% `p` is a term representing given program
% `s` is a term representing system state:
%     pc(ListOfCurrentProgramCountersForEveryProcess) - describes which
%         instruction should be called next for every process,
%     values(ListOfValuesForCorrespondingVariables) - values of system global
%         variables,
%     arrayValues(ListOfArrayValuesForCorrespondingArrays) - values of system
%         global arrays.
%     log(PerformedInstructions) - list of pairs in [PID, InstructionNo]
%         format.
initState(
    p(vars(Vars), arrays(As), program(_Insts)),
    N,
    s(pcs(PC), values(Vs), arrayValues(AVs), log([]))
) :-
    createList(N, 1, PC),
    length(Vars, VarsLength),
    createList(VarsLength, 0, Vs),
    length(As, AsLength),
    createMatrix(AsLength, N, 0, AVs).

% step(+Program, +InState, ?PID, -OutState)
step(
    p(vars(Vars), arrays(As), program(Insts)),
    s(pcs(PCs), values(Vs), arrayValues(AVs), log(H)),
    PID,
    s(pcs(NewPCs), values(NewVs), arrayValues(NewAVs), log([[PID, PC] | H]))
) :-
    nth0(PID, PCs, PC),  % Get current instruction pointer.
    nth1(PC, Insts, Inst),  % Get instruction to execute.
    stepInst(Inst, Vars, Vs, As, AVs, PID, PC, NewVs, NewAVs, NewPC),
    replace(PID, PCs, NewPC, NewPCs).

% replace(Index, Array, X, NewArray) :-
% `NewArray` is `Array` in which element at `Index` is replaced with `X`.
replace(Index, [E | Array], X, [E | NewArray]) :-
    Index > 0,
    Index0 is Index - 1,
    replace(Index0, Array, X, NewArray).
replace(0, [_E | Array], X, [X | Array]).

% `stepInst` "executes" given instruction for process PID.
stepInst(assign(Var, EV), Vars, Vs, As, AVs, PID, PC, NewVs, NewAVs, NewPC) :-
    NewPC is PC + 1,
    evalArithm(EV, Vars, Vs, As, AVs, PID, V),
    modifyVar(Var, V, Vars, Vs, As, AVs, PID, NewVs, NewAVs).

stepInst(goto(No), _Vars, Vs, _As, AVs, _PID, _PC, Vs, AVs, No).

stepInst(condGoto(BExp, No), Vars, Vs, As, AVs, PID, PC, Vs, AVs, NewPC) :-
    evalBool(BExp, Vars, Vs, As, AVs, PID) ->
        NewPC is No
    ;
        NewPC is PC + 1.

stepInst(section, _Vars, Vs, _As, AVs, _PID, PC, Vs, AVs, NewPC) :-
    NewPC is PC + 1.

% Modifies given environment, i.e. assigns `V` to `Var`.
modifyVar(Var, V, Vars, Vs, _As, AVs, _PID, NewVs, AVs) :-
    atom(Var),
    nth0(VarI, Vars, Var),
    replace(VarI, Vs, V, NewVs).

modifyVar(arr(Ident, IndexE), V, Vars, Vs, As, AVs, PID, Vs, NewAVs) :-
    evalArithm(IndexE, Vars, Vs, As, AVs, PID, Index),
    nth0(AI, As, Ident),
    nth0(AI, AVs, A),
    replace(Index, A, V, NewA),
    replace(AI, AVs, NewA, NewAVs).

% Succeeds if given boolean expression evaluates to True.
evalBool(EX < EY, Vars, Vs, As, AVs, PID) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    X < Y.
evalBool(EX = EY, Vars, Vs, As, AVs, PID) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    X = Y.
evalBool(EX <> EY, Vars, Vs, As, AVs, PID) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    X =\= Y.

% `R` is a value of `wyrArytm` ::= `wyrProste | wyrProste oper wyrProste`.
evalArithm(EX, Vars, Vs, As, AVs, PID, R) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, R).
evalArithm(EX + EY, Vars, Vs, As, AVs, PID, R) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    R is X + Y.
evalArithm(EX - EY, Vars, Vs, As, AVs, PID, R) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    R is X - Y.
evalArithm(EX * EY, Vars, Vs, As, AVs, PID, R) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    R is X * Y.
evalArithm(EX / EY, Vars, Vs, As, AVs, PID, R) :-
    evalSimple(EX, Vars, Vs, As, AVs, PID, X),
    evalSimple(EY, Vars, Vs, As, AVs, PID, Y),
    R is X + Y.

% `X` is a value of `wyrProste` `EX`.
evalSimple(Ident, Vars, Vs, _As, _AVs, PID, X) :-
    (number(Ident) ->
        X is Ident
    ; atom(Ident) ->
        (member(Ident, Vars) ->
            nth0(VarI, Vars, Ident),
            nth0(VarI, Vs, X)
        ;
            X is PID  % Every process knowns its PID.
        )
    ).

evalSimple(arr(Ident, IndexE), Vars, Vs, As, AVs, PID, X) :-
    evalArithm(IndexE, Vars, Vs, As, AVs, PID, Index),
    nth0(AI, As, Ident),
    nth0(AI, AVs, A),
    nth0(Index, A, X).

% createList(+N, +E, ?L)
% `L` is a list containing `N` elements `E`.
createList(N, E, [E | L]) :-
    N > 0,
    N0 is N - 1,
    createList(N0, E, L).
createList(0, _E, []).

% createMatrix(+M, +N, +E, ?L)
% `L` is a list containing `M` lists containging `N` elements `E`.
createMatrix(M, N, E, [L | T]) :-
    M > 0,
    M0 is M - 1,
    createList(N, E, L),
    createMatrix(M0, N, E, T).
createMatrix(0, _N, _E, []).

