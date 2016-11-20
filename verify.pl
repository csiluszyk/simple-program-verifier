#!/usr//bin/swipl -f -q
:- ensure_loaded(verifier).

:- initialization main.

main :-
  current_prolog_flag(argv, [ProcessesNo, File | _Argv]),
  atom_number(ProcessesNo, N),
  verify(N, File),
  halt(0).
main :-
  format('Usage: ./verify numberOfProcesses fileName~n'),
  halt(1).
