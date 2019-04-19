/* -*- Mode: Prolog -*- */
:- module(spacoj, [
	      spaces/1,
	      whites/1,
	      trim_left/2,
	      normalize/2,
	      trim_right/2,
	      linebreak/2
	  ]).

break_at(80).

t(1,'  \n \nab\n cd\n ').
t(2,'  \t\n \t ').
t(3,'\n').
t(4,'/o ').

test_trleft(N,T) :-
  t(N,A),
  trim_left(A,T).

test_normalize(N,T) :-
  t(N,A),
  normalize(A,T).

test_trright(N,T) :-
  t(N,A),
  trim_right(A,T).

normalize(Atom,NAtom) :-
  atom(Atom),
  atom_codes(Atom,Codes),
  normalize_(Codes,NCodes),
  atom_codes(NAtom,NCodes).

normalize_([Sp|T],[32|N]) :-
   code_type(Sp,space),!,
   skip(T,N).
normalize_([NonSp|T],[NonSp|N]) :-
   normalize_(T,N).
normalize_([],[]).

skip([Sp|T],N) :-
   code_type(Sp,space),!,
   skip(T,N).
skip(L,N) :- normalize_(L,N).


trim_left(Atom,Trimmed) :-
  atom(Atom),
  atom_codes(Atom,Codes),
  trim_left_(Codes,TrCodes),
  atom_codes(Trimmed,TrCodes).
trim_left_([Sp|Tail],Trimmed) :-
   code_type(Sp,space),!,
   trim_left_(Tail,Trimmed).
trim_left_(X,X).

trim_right(Atom,Trimmed) :-
  atom(Atom),
  atom_codes(Atom,Codes),
  length(Codes,Len),
  trim_right_(Codes,Len,TrCodes), !,
  atom_codes(Trimmed,TrCodes).

trim_right_(Codes,Pos,Trimmed) :-
   nth1(Pos,Codes,Sp),
   code_type(Sp,space), !,
   (Pos = 1 -> Trimmed = []
   ;
   Pos1 is Pos-1,
   trim_right_(Codes,Pos1,Trimmed)).
trim_right_(Codes,Pos,Trimmed) :-
   length(Trimmed,Pos),
   append(Trimmed,_,Codes).


spaces(Atom) :-
   atom(Atom),
   atom_codes(Atom,Codes),
   spaces(Codes).
spaces([Sp|Rest]) :-
  code_type(Sp,space),
  spaces(Rest).
spaces([]).


whites(Atom) :-
   atom(Atom),
   atom_codes(Atom,Codes),
   whites(Codes).
whites([W|Rest]) :-
  code_type(W,white),
  whites(Rest).
whites([]).




linebreak(Text,Broken) :-
  linebreak_(0,0,Text,Broken),!.

% se ni troviĝas ĉe la fino, ĉesu
linebreak_(_,_,[],[]).

% se ni troviĝas ĉe litersigno, iru al sekva
linebreak_(Pos,Indent,[C|Rest],[C|Brok]) :-
  \+ code_type(C,space),
  Pos1 is Pos+1,
  linebreak_(Pos1,Indent,Rest,Brok).

% se ni troviĝas ĉe spacsigno, ni kontrolos, ĉu
% necesas rompi tie ĉi, ĉar la sekva vorto estas tro longa
linebreak_(Pos,Indent,[C|Rest],[C1|Brok]) :-
  code_type(C,white),
  next_space(Offset,Rest),
  break_at(BreakAt),
  ( Pos + 1 + Offset > BreakAt
      % anstataŭigu la spacsignon per novlinio
      -> char_code('\n',C1), Pos1=Offset,
         format('pos: ~d offs: ~d~n',[Pos,Offset]), %dbg
         head_of_list(_,Brok,Indent,BrokRest)
      ; C1 = C, Pos1 is Pos+1+Offset, BrokRest = Brok
  ),
  % saltu je Offset kaj daŭrigu
  head_of_list(Offset,Rest,Next,RestTail),
  head_of_list(Offset,BrokRest,Next,BrokTail),
  linebreak_(Pos1,Indent,RestTail,BrokTail).

% se ni troviĝas ĉe linirompo, rekomencu nombri de nul
linebreak_(_Pos,_,[C|Rest],[C|Brok]) :-
  code_type(C,newline),
  % transsaltu spacsignojn (indent)
  skip_indent(Count,Rest),
  head_of_list(Count,Rest,Indent,RestTail),
  head_of_list(Count,Brok,Indent,BrokTail),
  linebreak_(Count,Indent,RestTail,BrokTail).


head_of_list(Count,List,Head,Tail) :-
  length(Head,Count),
  append(Head,Tail,List).


next_space(Offset,Text) :-
  next_sign_(Offset,Text,space,0).

%next_letter(Offset,Text) :-
%  next_sign_(Offset,Text,graph,0).

skip_indent(Count,Text) :-
  skip_signs_(Count,Text,white,0).


skip_signs_(Count,[C|Text],Type,Count0) :-
  code_type(C,Type),
  Count1 is Count0 + 1,
  skip_signs_(Count,Text,Type,Count1),!.

skip_signs_(Count,_,_,Count).

next_sign_(Offset,[C|Text],Type,Offset0) :-
  \+ code_type(C,Type),
  Offset1 is Offset0 + 1,
  next_sign_(Offset,Text,Type,Offset1).

next_sign_(Offset,[C|_],Type,Offset) :-
  code_type(C,Type).

next_sign_(Offset,[],_,Offset).



