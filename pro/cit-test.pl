/* -*- Mode: Prolog -*- */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(debug)).
% >>>>>>>>>>>>>>> this needs to be in place to cause the increasing VM-code memory error
thread_init :-   debug(http(request)).
:- thread_initialization(thread_init).

:- initialization(main,main).
:- http_handler('/', sercho, []).

port(9999).                                  % other number of workers, e.g. 2 has same result
main :- port(Port), http_server(http_dispatch, [port(Port),workers(10)]), test(100), prolog.

test(Max) :- test(Max,Max).
test(Max,T) :- T>0, N is Max-T+1, format('>>> TEST ~d <<<~n',[N]), http_request, T1 is T-1, test(Max,T1).
test(_,0).    
    
http_request :- port(Port), format(atom(Url),'http://localhost:~d/',[Port]),
    http_open(Url,Stream,[]), read_stream_to_codes(Stream,C),
    length(C,L), format('got ~d bytes from HTTP request~n',[L]), close(Stream).

sercho(_) :- format('Content-type: text/plain~n~n'),
    time(concurrent_maplist(atom,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])), write(return).

