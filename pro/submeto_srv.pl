/* -*- Mode: Prolog -*- */
:- module(submeto_srv,
	  [ 
        submeto/5, % (Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Quoted)
        subm_pluku/2, % (Id,State)
        subm_rezulto/3, %Id, State, Result
        subm_statoj/2, % (Format,Email), Format: json|html
        subm_malnovaj_for/0, % (Tagoj)
        subm_listo_novaj/1 % text|html
	  ]).

:- use_module(library(debug)).
%:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(pro(db/submetoj)).
:- use_module(pro(db/util)).

subm_listo_max(20).

csv_escape(Str,Escaped) :-
    replace_atom(Str,'\n','\\n',Esc1),
    replace_atom(Esc1,'\r','\\r',Esc2),
    replace_atom(Esc2,'\t','\\t',Esc3),
    replace_atom(Esc3,'"','""',Escaped).


submeto(Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Quoted) :-
    %atom_codes(Xml,Quoted),
    hex_bytes(Hex,Quoted),
    submeto_add(Retadreso,Redakto,Shangho_au_Nomo,Dosiero,Hex).

subm_listo_novaj(text) :-
    debug(submeto(novaj),subm_listo_novaj,[]),
    format('Content-type: text/plain; charset=utf-8~n~n'),
    forall(
        subm_listo_novaj_db(Listo),
        (
            atomic_list_concat(Listo,';',Line),
            write(Line)
        )
    ).

subm_listo_novaj(html) :-
    debug(submeto(novaj),subm_listo_novaj,[]),
    format('Content-type: text/html; charset=urf-8~n~n'),
    write('<html><pre>'),
    forall(
        subm_listo_novaj_db([Id|Cetero]),
        (
        %debug(submeto(novaj),'Id: ~q',[Id]),
        atomic_list_concat(['<a href="submeto.pl?id=',Id,'">',Id,'</a>'],Ref),
        atomic_list_concat([Ref|Cetero],';',Line),    
        debug(submeto(novaj),'Line: ~q',[Line]),
        write(Line)
        )
    ),
    write('</pre></html>').

subm_listo_novaj_db(Listo) :-
    submetoj_by_state('nov',Row), 
    debug(submeto(novaj),'~q',[Row]),
    Row =.. [row|L],
    % sub_id,sub_time,sub_state,sub_email,sub_cmd,sub_desc,sub_fname
    % protektu specialajn signojn en desc
    nth1(6,L,Desc,Rest),
    %debug(submeto(novaj),'Desc: ~q',[Desc]),
    csv_escape(Desc,Esc),
    atomic_list_concat(['"',Esc,'"'],Quoted),
    %debug(submeto(novaj),'Quoted: ~q',[Quoted]),
    nth1(6,Listo,Quoted,Rest).


subm_pluku(Id,State) :-
    debug(submeto(subm_pluku),subm_pluku,[]),
    submeto_by_id(Id,Row),
    Row =.. [row|Subm],
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
    debug(submeto(subm_rezulto),subm_rezulto,[]),
    format('Content-type: text/plain; charset=utf-8~n~n'),
    submeto_update(Id,State,Result),
    writeln('1').

subm_statoj(json,Email) :-
    debug(submeto(subm_statoj),subm_statoj,[]),
    subm_listo_max(Max),

   %    debug(submeto(novaj),subm_listo_novaj),
   % {
   %     "id":"32b209bbf7fb98bb6fcdc1da23a1c47a",
   %     "desc":"redakto:+ekz-oj, bld",
   %     "created":"2025-05-05T05:37:10Z",
   %     "updated":"2025-05-05T06:40:19Z",
   %     "name":"smeral.xml",
   %     "html_url":"https://gist.github.com/reta-vortaro/32b209bbf7fb98bb6fcdc1da23a1c47a",
   %     "xml_url":"https://gist.githubusercontent.com/reta-vortaro/32b209bbf7fb98bb6fcdc1da23a1c47a/raw/59cd9c6068865fd76c10556bd4e68cd7d11ef65a/smeral.xml",
   %     "rezulto":"konfirmo",
   %     "rez_url":"https://gist.githubusercontent.com/reta-vortaro/32b209bbf7fb98bb6fcdc1da23a1c47a/raw/d3fad56d910a6fe51e4045fa84aafbc8822253ef/konfirmo.json"
   %   },

    findall(_{
        id: Id, desc: Desc, created: Time, updated: Time, 
        name: FName, html_url: '', xml_url: '', rezulto: Result, rez_url: ''},
        submetoj_by_email(Email,row(Id, Time, State,_Email,_Cmd, Desc, FName, Result),Max),
        Submetoj),
    reply_json(Submetoj).

subm_statoj(html,Email) :-
        debug(submeto(subm_statoj),subm_statoj,[]),
        subm_listo_max(Max),
        format('Content-type: text/html; charset=urf-8~n~n'),    
        write('<html><table>'),
        forall(
            submetoj_by_email(Email,Row,Max),
            (
               Row =.. [row|Subm],
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
    debug(submeto(subm_malnovaj_for),subm_malnovaj_for,[]),
    format('Content-type: text/plain; charset=utf-8~n~n'),
    submetoj_delete,
    writeln('1').

   