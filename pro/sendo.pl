/* -*- Mode: Prolog -*- */
:- module(sendo, [
	      send_file/4,
	      send_codes/4,
	      send_revo_redakto/3,
	      send_revo_aldono/3
   ]).

:- consult(library(ssl)).
:- use_module(smtp).
:- use_module(pro(cfg/agordo)).


xml_file_test('abel.xml').

stream_codes(Codes,Prolog,Out) :-
    set_stream(Out,encoding(utf8)),
    write(Out,Prolog), 
    nl(Out), nl(Out),
    %format('~s',[Codes]).
    atom_codes(Text,Codes),
    write(Out,Text).


stream_file(InFile,Prolog,Out) :-
    setup_call_cleanup(
	(
	    set_stream(Out,encoding(utf8)),
	    open(InFile,read,InStream,[encoding(utf8)])
	),
	(
	    write(Out,Prolog), 
	    nl(Out), nl(Out),
	    copy_stream_data(InStream,Out)
	),
	close(InStream)).

send_file(File,ToAddr,Prolog,MailOptions) :-
    smtp_send_mail(ToAddr,stream_file(File,Prolog),MailOptions).

send_codes(Codes,ToAddr,Prolog,MailOptions) :-
    smtp_send_mail(ToAddr,stream_codes(Codes,Prolog),MailOptions).


send_revo_redakto(Redaktanto,Shangho,Artikolo) :-
    % sendu
    mail_options(Options),
    agordo:get_config(mail_addr,RevoAddr),
    %% format(atom(To),'<~w>,<~w>',[RevoAddr,Redaktanto]),
    atom_concat('redakto: ',Shangho,Prolog),
    append(Options,[subject(Shangho)],MailOpts),
    append(MailOpts,[header('Reply-To'(Redaktanto))],MailOpts1),
    % sendu al Revoservo
    debug(sendo,'to = ~q, reply-to = ~q',[RevoAddr,Redaktanto]),
    send_codes(Artikolo,RevoAddr,Prolog,MailOpts1),
    % sendu kopion al la Redaktanto
    debug(sendo,'to = ~q',[Redaktanto]),
    send_codes(Artikolo,Redaktanto,Prolog,MailOpts).

send_revo_aldono(Redaktanto,Dosieronomo,Artikolo) :-
    % sendu
    mail_options(Options),
    agordo:get_config(mail_addr,RevoAddr),
    %% format(atom(To),'<~w>,<~w>',[RevoAddr,Redaktanto]),
    atom_concat('aldono: ',Dosieronomo,Prolog),
    append(Options,[subject(Prolog)],MailOpts),
    append(MailOpts,[header('Reply-To'(Redaktanto))],MailOpts1),
    % sendu al Revoservo
    debug(sendo,'to = ~q, reply-to = ~q',[RevoAddr,Redaktanto]),
    send_codes(Artikolo,RevoAddr,Prolog,MailOpts1),
    % sendu kopion al la Redaktanto
    debug(sendo,'to = ~q',[Redaktanto]),
    send_codes(Artikolo,Redaktanto,Prolog,MailOpts).


mail_options(Options) :-
    agordo:get_config([
	mail_from(From),
	mail_smtp(Server),
	mail_pw(Pwd),
        mail_auth(Method),
	mail_security(Sec),
	mail_user(User)
    ]),
    Options = [
	from(From),
	smtp(Server),
	security(Sec),
        auth_method(Method),
	auth(User-Pwd)].

test_send_file :-
    xml_file_test(File),

    mail_options(Options),
    agordo:get_config([
	mail_addr_test(To),
	mail_reply_test(Reply)]),

  Prolog = "redakto: nenio, nur testo",

%  format('Donu pasvorton  por poŝtfako ~w:',[User]),
%  read_line_to_codes(user_input,Pwd), % or current_input?
  %  atom_codes(PwdAtom,Pwd),
  %get_pwd(PwdAtom),
  
  append(Options,[subject(File),header('Reply-To'(Reply))],MailOpts),
  send_file(File,To,Prolog,MailOpts).


