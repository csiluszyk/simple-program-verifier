# Thread-safty verifier

Thread safty verifier of simple programs.

## Project structure
* `verify.pl` - program runner,
* `verifier.pl` - source code of the verifier,
* `dekker.pl` - [Dekker's algorithm][0] example,
* `hyman.pl` - [Hyman's mutual exclusion algorithm][1] example,
* `peterson.pl` - [Peterson's algorithm][2] example,
* `unsafe.pl` - thread-unsafe example,
* `example.pl` - thread-safe if number of processors <= 2 example.

## Grammar of simple programs
```
Exp         ::=  ExpVal | ExpVal ExpOper ExpVal
ExpVal      ::=  number | Variable
Variable    ::=  ident | arr(ident, Exp)
ExpOper     ::=  + | - | * | /
BoolExp     ::=  ExpVal BoolOper ExpVal
BoolOper    ::=  < | = | <>
```

## Running tests
### Requirements
* [SWI-Prolog][3]

### Example runs:
* `./verify.pl 2 peterson.pl` - correct,
* `./verify.pl 3 peterson.pl` - incorrect,


[0]: https://en.wikipedia.org/wiki/Dekker's_algorithm
[1]: http://babel.ls.fi.upm.es/~fred/uppaal/hyman.html
[2]: https://en.wikipedia.org/wiki/Peterson's_algorithm
[3]: http://www.swi-prolog.org/
