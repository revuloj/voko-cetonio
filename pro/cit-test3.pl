/* -*- Mode: Prolog -*- */
%:- module(citajho_servo,  [  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)). % reading post data
:- use_module(library(http/http_session)).
%:- use_module(library(http/json)).
%:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).

:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_open)).
:- use_module(library(settings)).
:- use_module(library(xpath)).

:- multifile http:location/3.
:- dynamic   http:location/3.

% TODO: is http/http_error autoloaded?
% see http://www.swi-prolog.org/pldoc/man?section=http-debug

:- use_module(library(debug)).
%%%%:- use_module(ekzfnt).
:- use_module(tekstaro).

:- debug(http(request)).
:- debug(sercho(what)).
:- debug(sercho(stats)).
%:- debug(openid(_)).

port(9090).

% aktuale la demono malŝaltas protokoladon per "debug"
% kaj ŝalto per komandlinio ne funkcias...
thread_init :-   debug(http(request)).

:- initialization(init).
:- initialization(main,main).
:- thread_initialization(thread_init).

init :-
    set_prolog_flag(encoding,utf8),
    set_setting(http:prefix,'/citajhoj'),
    set_setting(http:public_scheme,http),
    port(Port),
    set_setting(http:public_port,Port),
    set_setting(http:public_host,localhost),
    http_set_session_options([
	cookie(redaktilo_seanco),
	timeout(3600),
	path('/citajhoj')
	])
    .

% redirect from / to /citajhoj/, when behind a proxy, this is a task for the proxy
:- http_handler('/', http_redirect(moved,root(.)),[]).
:- http_handler(root(citajho_sercho), citajho_sercho, []).
main :- port(Port), http_server(http_dispatch, [port(Port),workers(10)]), prolog. % test(100),prolog.


/*******************************************/

test(Max) :- test(Max,Max).

test(Max,T) :-
    T>0,
    N is Max-T+1,
    format('>>> TEST ~d <<<~n',[N]),
    http_request('kepleraj leĝoj'),
    T1 is T-1,
    test(Max,T1).

test(_,0).    
    

http_request(Text) :-
    Kie=klasikaj,
    port(Port),
    uri_authority_components(Auth,uri_authority(_,_,localhost,Port)),
    uri_query_components(Search,[sercho(Text),kie(Kie)]),
    uri_components(Url,uri_components(http,Auth,'/citajhoj/citajho_sercho',Search,_)),
    %    http_open(Url,Stream,[header(content_type,'text/plain')]),
    http_open(Url,Stream,[]),
%    format('Content-type: ~w~n~n',[ContentType]),
    set_stream(Stream,encoding(utf8)),
    read_stream_to_codes(Stream,C),
    length(C,L),
    format('got ~d bytes from HTTP request~n',[L]),
    close(Stream).


citajho_sercho(Request) :-
%%    ajax_auth(Request),
    debug(redaktilo(auth),'permesite',[]),
    http_parameters(Request,
	    [
	    sercho(Sercho, [length>1,length<500]),
	    kie(Kie, [oneof([vikipedio,anaso,klasikaj,postaj])]) 
	    ]),
    sercho(Kie,Sercho).


sercho(klasikaj,Sercho) :-
    format('Content-type: text/plain~n~n'),
    time(findsmart(50,klasikaj,Sercho,Result)),
    set_stream(current_output,encoding(utf8)),
    write(Result).


findsmart(Max,Verkaro,Sercho,Trovoj) :-
  verkaro(Verkaro,Verkoj),
  atom_length(Sercho,L), L>3,
  %concurrent_maplist(findbest(ngram,Max,Sercho),Verkoj,T),
  maplist(findbest(ngram,Max,Sercho),[[pv, mt, nt], [fb, gf, fr, fe]],T),
  
  % faru unu liston el pluraj (pro paralela serĉo)
  append(T,Trv),
  group_by(_,Simil-T1,
	   limit(Max,
		 order_by([desc(Simil)],member(Simil-T1,Trv))),
	   Trovoj).
%  cit_to_ekzj(Trovj,Trovoj).


findbest(Method,Max,Sercxo,Verkoj,Trovoj) :-
    is_list(Verkoj),
    group_by(_, Simil-cit(Vrk:Dos,Lok,Txt),
	     limit(Max,
		   order_by([desc(Simil)],
			    (
				member(Vrk,Verkoj),
				find(Method,Sercxo,cit(Vrk:Dos,Lok,Txt),Simil)
			    ))),
	     Trovoj), !.

% empty list when nothing found
findbest(_,_,_,_,[]).


find(ngram,Sercxo,cit(Vrk:Dos,Lok,Txt),Simil) :-
    atom_length(Sercxo,L), L>3,
    ngrams(Sercxo,4,NGrams),
    tekstaro:cit(Vrk:Dos,Lok,Txt),
    ngram_find(NGrams,Txt,Simil),
    Simil > 0.3. %0.3 .


ngram_find(NGrams,Atom,Percentage) :-
    proper_length(NGrams,Len), Len>0,
    ngram_count(NGrams,Atom,0,Count),
    Percentage is Count / Len.   % >= 0.7

ngram_count([],_,C,C).
ngram_count([NGram|More],Atom,C,Count) :-
    once((
	sub_atom(Atom,_,_,_,NGram),
	C1 is C+1
	;
	C1=C
    )),
    ngram_count(More,Atom,C1,Count).
  
ngrams(Atom,Len,NGrams) :- 
    setof(NGram,A^B^sub_atom(Atom,B,Len,A,NGram),NGrams).
