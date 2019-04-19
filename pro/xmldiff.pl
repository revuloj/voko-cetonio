
diff(element(Tag,Attr1,Content1),element(Tag,Attr2,Content2)):-
	diff_attr(Attr1,Attr2),
	diff(Content1,Content2).

diff([C|Rest1],[C|Rest2]) :- !,
	diff(Rest1,Rest2).

diff([element(Tag,Attr1,Content1)|Rest1],[element(Tag,Attr2,Content2)|Rest2]):- 
	diff_attr(Attr1,Attr2), !,
	diff(Content1,Content2),
	diff(Rest1,Rest2).

%diff([W1|Rest1],[W2|Rest2]) :-
%	blanks(W1),
%	blanks(W2),
%	diff(Rest1,Rest2).

diff(Content1,Content2) :-
	spaces_before_element_or_end(Spaces1,Rest1,Content1),
	spaces_before_element_or_end(Spaces2,Rest2,Content2),
	(Spaces1 \= [] ; Spaces2 \= []), !,
	diff(Rest1,Rest2).

diff([],[]) :- !.

% diferencoj inter enhavo

diff([X|Rest1],[Y|Rest2]) :-
	X \= Y, !,
	format('ORIG: >>>~w<<<~nNOVA: >>>~w<<<~n',[X,Y]),
	diff(Rest1,Rest2).

diff(X,Y) :-
	X \= Y, !,
	format('ORIG: >>>~w<<<~nNOVA: >>>~w<<<~n',[X,Y]).



diff_attr(A,A).


spaces_before_element_or_end([S|Spaces],ElementContent,[S|Rest]) :-
	blanks(S),
	spaces_before_element_or_end(Spaces,ElementContent,Rest).

%spaces_before_element_or_end([S|Spaces],ElementContent,[content(S)|Rest]) :-
%	blanks(S),
%	spaces_before_element_or_end(Spaces,ElementContent,Rest).

spaces_before_element_or_end([],ElCnt,ElCnt) :-
	ElCnt = [element(_,_,_)|_].

spaces_before_element_or_end([],[],[]).


blanks(Atom) :-
   atom(Atom),
   atom_codes(Atom,Codes),
   blanks(Codes).
blanks([W|Rest]) :-
  code_type(W,space),
  blanks(Rest).
blanks([]).

