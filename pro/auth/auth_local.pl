:- module(auth_local,[]).

% loka uzanto agordita, tio funkcias kun malplena datumbazo de redaktantoj kaj aparta saltuo-provizanto (OAuth2...)

:- use_module(pro(cfg/agordo)).
:- use_module(pro(param_checks)).
:- use_module(pro(db/redaktantoj)). % nur email_redid uzata por kalkuli "hash"

:- multifile http:authenticate/3.
http:authenticate(local,Request,[user(User),email(Email)]) :-  local_auth(Request,User,Email).

local_auth(_Request,User,Email) :-
    getenv('REDAKTANTO_RETPOSHTO',Email),
    check_email(Email,CheckedEmail), 
    email_redid(CheckedEmail,User). % USER = hash(Email)

