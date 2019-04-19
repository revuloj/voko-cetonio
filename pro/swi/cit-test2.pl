:- use_module(library(debug)).
thread_init :-   debug(ctest).
:- thread_initialization(thread_init).

test(Times) :-
    Times>0,
    concurrent_maplist(output,[Times]),
    T1 is Times-1,
    test(T1).

output(N) :-
    sleep(3),
    debug(ctest,'~d ',[N]).

