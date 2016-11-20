% n=<2 correct
vars([x]).
arrays([]).
program([
	assign(x, pid),
	condGoto(x < pid, 4),
	goto(5),
	section,
	goto(1)
]).
