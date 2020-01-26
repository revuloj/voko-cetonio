:- module(db_util, [
        replace_apos/2,
        replace_atom/4
    ]).

replace_apos(In,Out) :-
    replace_atom(In,'''','''''',Out).

replace_atom(In,From,To,Out) :-
    atom_codes(From,FromList),
    atom_codes(To,ToList),
    atom_codes(In,InList),
    replace(InList,FromList,ToList,OutList),
    atom_codes(Out,OutList).

replace(InList,From,To,OutList) :-
    append(From,Rest,InList),!,
    replace(Rest,From,To,RestOut),
    append(To,RestOut,OutList).

replace([X|RestIn],From,To,[X|RestOut]) :-
    replace(RestIn,From,To,RestOut).

replace([],_,_,[]).