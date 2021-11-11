:- module(auth_local,[]).

% Kiam ni uzas la redaktilon loke, ni ne volas uzi registron de redaktantoj
% kaj apartan servon por ensaluti. La solan uzanton ni simple transdonas
% kiel retpoŝta adreso en mediovariablo REDAKTANTO_RETPOSHTO
% Noto: tamen devas esti registrita retpoŝtadreso, se la redaktoj
% traktiĝu poste!

:- use_module(pro(cfg/agordo)).
:- use_module(pro(param_checks)).
:- use_module(pro(db/redaktantoj)). % nur email_redid uzata por kalkuli "hash"

:- multifile http:authenticate/3.
http:authenticate(local,Request,[user(User),email(Email)]) :- local_auth(Request,User,Email).

local_auth(_Request,User,Email) :-
    getenv('REDAKTANTO_RETPOSHTO',Email),
    debug(auth,'>> local_auth: ~q',[Email]),	   
    check_email(Email,CheckedEmail), 
    email_redid(CheckedEmail,User). % USER = hash(Email)

