/* -*- Mode: Prolog -*- */

:- use_module(library(http/http_open)).
:- use_module(library(http/http_session)).

:- use_module(auth_google).

%:- use_module(redaktilo_auth).
%:- use_module('redaktilo-servo.pl').


test_reg(0) :-
    %    server(8080),
        http_open('http://localhost:8080/login?server=google&retposhto=test%40example.com',Stream,[status_code(Status)]),
        read_stream_to_codes(Stream,Codes),
        format('HTTP-~w ~s~n',[Status,Codes]),
        close(Stream).

test_reg(1) :-
%    server(8080),
    http_open('http://localhost:8080/redaktilo/reg/revo_registro1?retposhto=test%40example.com',Stream,[status_code(Status)]),
    read_stream_to_codes(Stream,Codes),
    format('HTTP-~w ~s~n',[Status,Codes]),
    close(Stream).

test_reg(2) :-
%    server(8080),
    http_open('http://localhost:8080/redaktilo/reg/revo_registro1?retposhto=test%40example.com%20',Stream,[status_code(Status)]),
    read_stream_to_codes(Stream,Codes),
    format('HTTP-~w ~s~n',[Status,Codes]),
    close(Stream).

   
test_reg(3) :-
%    server(8080),
    http_open('http://localhost:8080/redaktilo/reg/revo_registro1?retposhto=<test%40example.com>',Stream,[status_code(Status)]),
    read_stream_to_codes(Stream,Codes),
    format('HTTP-~w ~s~n',[Status,Codes]),
    close(Stream).
 
test_reg(4) :-
    %    server(8080),
    http_session_assert(openid('112232376668002576720')),
    http_open('http://localhost:8080/redaktilo/reg/revo_registro1?retposhto=wolfram%40steloj.de',Stream,[status_code(Status)]),
    read_stream_to_codes(Stream,Codes),
    format('HTTP-~w ~s~n',[Status,Codes]),
    close(Stream).
