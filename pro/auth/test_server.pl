:- use_module(library(http/http_unix_daemon)).

:- use_module(server_auth).
:- consult('oauth_setup').

:- initialization(run, main).
:- debug(http(_)).

run :-
    http_daemon([port(3040),interactive(true)]).
