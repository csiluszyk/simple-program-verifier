vars([k]).
arrays([chce]).
program([assign(arr(chce, pid), 1), assign(k, pid),
        condGoto(arr(chce, 1-pid) = 0, 5),
        condGoto(k = pid, 3),
        section, assign(arr(chce, pid), 0), goto(1)]).
