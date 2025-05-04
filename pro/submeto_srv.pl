/* -*- Mode: Prolog -*- */
:- module(submeto_srv,
	  [ 
        subm_pluku/2, % (id)
        subm_rezulto/3, %Id, State, Result
        subm_statoj/1, % (email)
        subm_malnovaj_for/0, % (tagoj)
        subm_listo_novaj/1 % text|html
	  ]).

%:- use_module(library(debug)).
%:- use_module(library(http/html_write)).
:- use_module(pro(db/submetoj)).
:- use_module(pro(db/util)).

subm_listo_max(20).

csv_escape(Str,Escaped) :-
    replace_atom(Str,'\n','\\n',Esc1),
    replace_atom(Esc1,'\r','\\r',Esc2),
    replace_atom(Esc2,'\t','\\t',Esc3),
    replace_atom(Esc3,'"','""',Escaped).

subm_listo_novaj(text) :-
    format('Content-type: text/plain; charset=utf-8~n~n'),
    forall(
        subm_listo_novaj_db(Listo),
        (
            atomic_list_concat(Listo,';',Line),
            write(Line)
        )
    ).

subm_listo_novaj(html) :-
    format('Content-type: text/html; charset=urf-8~n~n'),
    write('<html>'),
    forall(
        subm_listo_novaj_db([Id|Cetero]),
        (
        atomic_list_concat(['<a href="submeto.pl?id=',Id,'">',Id,'</a>'],Ref),
        atomic_list_concat([Ref|Cetero],';',Line),    
        write(Line)
        )
    ),
    write('</html>').

subm_listo_novaj_db(Listo) :-
    submetoj_by_state('nov',Listo), 
    % protektu specialajn signojn en desc
    nth1(6,Listo,Desc,Rest),
    csv_escape(Desc,Esc),
    atomic_list_concat(['"',Esc,'"'],Quoted),
    nth1(6,Listo,Quoted,Rest).


subm_pluku(Id,State) :-
    submeto_by_id(Id,Subm),
    %sub_time,sub_state,sub_email,sub_cmd,sub_desc,sub_fname,sub_content
    nth1(3,Subm,Email),
    nth1(7,Subm,Content),
    nth1(2,Subm,SubState),
    format('Content-type: text/plain; charset=utf-8~n~n'),
    format('From: ~w~n~n',[Email]),
    write(Content),
    once((
        State = trakt,
        SubState = nov,
        submeto_update(Id,trakt,'')
        ;
        true
    )).

subm_rezulto(Id, State, Result) :- 
    format('Content-type: text/plain; charset=utf-8~n~n'),
    submeto_update(Id,State,Result),
    writeln('1').

subm_statoj(Email) :-
    subm_listo_max(Max),
    format('Content-type: text/html; charset=urf-8~n~n'),    
    write('<html><table>'),
    forall(
        submetoj_by_email(Email,Subm,Max),
        (
           % sub_id,sub_time,sub_state,sub_email,sub_cmd,sub_desc,sub_fname,sub_result
           % -> sub_id,sub_fname,sub_state,sub_time,sub_desc,sub_result
           nth1(1,Subm,Id),
           nth1(2,Subm,Time),
           nth1(3,Subm,State),
           nth1(6,Subm,Desc),
           nth1(7,Subm,FName),
           nth1(8,Subm,Result),
           format('<tr><td>~w</td><td>~w</td><td>~w</td><td>~w</td><td>~w</td><td>~w</td></tr>',[Id,FName,State,Time,Desc,Result])
        )
    ),
    write('</table></html>').

subm_malnovaj_for :-
    format('Content-type: text/plain; charset=utf-8~n~n'),
    submetoj_delete,
    writeln('1').

   