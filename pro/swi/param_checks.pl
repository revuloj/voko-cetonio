/* -*- Mode: Prolog -*- */
:- module(param_checks,
	  [
	      check_email/2,
	      email//1,
	      check_url/1,
	      check_int/1,
	      check_hash/1,
	      check_search/1
       ]).
			    
:- use_module(library(dcg/basics)).

check_email(Email,Checked) :-
   once((
     atom_codes(Email,Codes),
     phrase(email(Checked),Codes)
     ;		  
     throw(invalid_email_param(Email))
   )).

email(Email) --> whites, "<", !, email_part(Left), "@", email_part(Right), ">", whites, { atomic_list_concat([Left,Right],'@',Email) }.
email(Email) --> whites, email_part(Left), "@", email_part(Right), whites, { atomic_list_concat([Left,Right],'@',Email) }.

email_part(Part) --> email_part_(P), { atom_codes(Part,P), atom_length(Part,L), L > 1 }.

email_part_([C|Part]) --> email_char(C), email_part_(Part).
email_part_([]) --> [].

% FIXME: laŭ https://en.wikipedia.org/wiki/Email_address#Local-part
% antaŭ @ estas permesataj ankaŭ: !#$%&'*+-/=?^_`{|}~;
% sed . ne rajtas esti unue, unu-literaj  maldekstraj partoj permesitaj 
% la parto post @ ankaŭ rajtas enhavi UTF-8-koditajn ne-Askiajn literojn
% momente ni ne permesas tiajn retadresojn!

email_char(0'_) --> "_", !.
email_char(0'_) --> "-", !.
email_char(0'.) --> ".", !.
email_char(C) --> digit(C).
email_char(C) --> [C], {C >= 0'a, C =< 0'z}.
email_char(L) --> [C], { C >= 0'A, C =< 0'Z, to_lower(C,L) }.

check_url(Url) :-
   parse_url(Url,_) -> true
   ;
   throw(invalid_url(Url)).

check_int(Digits) :-
    once((
	atom_codes(Digits,Codes),
	phrase(digits(_),Codes)
        ;
	throw(invalid_number(Digits))
    )).


check_hash(Hash) :-
    once((
       atom_length(Hash,20),
       atom_codes(Hash,Codes),
       phrase(xinteger(_),Codes)
     ; throw(invalid_hash(Hash))
       )).

check_search(Kion) :-
    atom_codes(Kion,Codes),
    \+ memberchk(39,Codes), % apostroph: '
    \+ memberchk(92,Codes), % backslash: \
    \+ memberchk(59,Codes)  % semicolon: ;
    -> true
    ;
    throw(invalid_search_word(Kion)).
