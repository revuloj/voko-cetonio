/* -*- Mode: Prolog -*- */

:- use_module(library(dcg/basics)).
:- consult(vrtsign).

test(1,[text(x),text(y),invalid,text(z)]).

test_content(N,Tree,L) :-
  test(N,List),
  phrase(content(Tree),[1|List],[L]).

content([]) --> [].

content([Text|Rest]) --> 
	[text(Text)], 
	content(Rest).

content(Content) --> !, % ne rekuru post eraro!
	call(parse_error_('nevalida enhavo')), % !, % ne rekuru post kreo de eraro
 	resync_content(Content), !. % ne rekuru post resync


parse_error_(Msg,[Line|Rest],[Line|Rest]) :-
  once(( % ne kreu saman eraron diversmaniere...
	first_part_of_list(20,Rest,Begin),
% debugging...
	output_error(Line,Msg,Begin), 
	assertz(vrt_parse_error(Line,Msg,Begin))
  )).
%  throw(invalid_content_at(Line,Begin)).


resync_content([]), [Line] --> 
	[Line], 
	[_], string_without([nl(_),lt,rbr],_).
%	set_line(Line), 
%	content(Content).


output_error(Line,Msg,ErrList) :-
  once((
   format('% ERARO ĉe linio ~d: ~w:~n',[Line,Msg]),
   output(ErrList),
   format('~n')
  )).
