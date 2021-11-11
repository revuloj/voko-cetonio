:- module(gist,[
    post_gist/5,
    get_gists/1
]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(crypto)).
:- use_module(agordo).

gh_gist_url('https://api.github.com/gists').
gh_owner('reta-vortaro').


post_gist(Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Quoted) :-
    agordo:get_config(github_token,AccessToken),
    atom_codes(Xml,Quoted),
    sigelo(Retadreso,Xml,Sigelo),
    info_json(Retadreso,Sigelo,InfoJson),
    atom_json_term(Info,InfoJson,[]),
    json_post_data(Redakto,Dosiero,Shangho_au_Nomo,Info,Xml,JsonData),
    gh_gist_url(URL),
    atom_concat('token ',AccessToken,TokenHeader),
    http_post(URL, json(JsonData), Reply, 
        [redirect(false),authenticate(false),request_header(authorization=TokenHeader),
        status_code(Status)]),
    once((
        Status = 201,
        gist_html_url(Reply,JsonUrl),
        reply_json(JsonUrl,[status(Status)])
        ;
        reply_json(Reply,[status(Status)])
    )).

get_gists(Retadreso) :-
  list_gist(Retadreso,Gists),
  reply_json(Gists).

list_gist(Retadreso,Gists) :-
  agordo:get_config(github_token,AccessToken),
  gh_gist_url(URL), atom_concat('token ',AccessToken,TokenHeader),
  http_get(URL, Reply, 
        [redirect(false),authenticate(false),request_header(authorization=TokenHeader)]),!,  
  json_gist_list(Retadreso,Reply,Gists),!.


json_gist_list(_,[],[]).
json_gist_list(Retadreso,[json(Json)|Rest],[Gist|MoreGists]) :-
  member(id=Id,Json),
  debug(gist,'gist: ~q',[Id]),
  member(files=json(FilesJson),Json),
  member(FName=json(JFile),FilesJson),
  debug(gist,'  file: ~q',[FName]),
  FName \= 'rezulto.json',
  FName \= 'rezulto.log',
  member(type='application/json',JFile),
  member(raw_url=RawUrl,JFile),
  gist_autoro(Retadreso,RawUrl),
  gist_list_item(Json,Gist),
  debug(gist,'  item: ~q~n',[Gist]),
  json_gist_list(Retadreso,Rest,MoreGists).
% skip
json_gist_list(Retadreso,[_|Rest],MoreGists) :-
  debug(gist,'  SKIP',[]),
  json_gist_list(Retadreso,Rest,MoreGists).

gist_autoro(Retadreso,Url) :-
  http_get(Url, Reply, []),
  %format('    jr:~w~n',[Reply]),
  atom_json_term(Reply,json(J),[]),
  member(red_adr=Retadreso,J).

gist_list_item(Json,json([id=Id,desc=Desc,created=Created,updated=Updated,name=XmlName,url=XmlUrl])) :-
  member(id=Id,Json),
  member(description=Desc,Json),
  member(created_at=Created,Json),
  member(updated_at=Updated,Json),
  member(files=json(FilesJson),Json),
  member(XmlName=json(JFile),FilesJson),
  member(type='application/xml',JFile),
  member(raw_url=XmlUrl,JFile).


/***
{
  "description": "testo",
  "files": {
    "${fname}": {
      "content": "${xml}"
    },
    "info.json": {
      "content": "${info}"
    }
  }
}
***/
json_post_data(Redakto,Dosiero,Shangho_au_Nomo,Info,Xml,
    json([description=Desc,files=Files])) :-        
    %escape_data(Info,InfoEsc),
    %escape_data(Xml,XmlEsc),
    atomic_list_concat([Redakto,Shangho_au_Nomo],':',Desc),
    atom_concat(Dosiero,'.xml',XmlDos),
    atom_concat(Dosiero,'.json',JsnDos),
    Files=json([
        XmlDos=json([content=Xml]),
        JsnDos=json([content=Info])
        ]).


info_json(Retadreso,Sigelo,json([red_adr=Retadreso,sigelo=Sigelo,celo=Celo])) :-
    agordo:get_config(github_repo,Repo),
    atom_concat(Repo,'/revo',Celo).
    
sigelo(Retadreso,Quoted,Sigelo) :-
    agordo:get_config(sigelilo,Sigelilo),
    atomic_list_concat([Retadreso,Quoted],'\n',Data),
    crypto_data_hash(Data,Sigelo,[algorithm(sha256),hmac(Sigelilo)]).

gist_html_url(json(KV),json([html_url=Url])) :-
    member(html_url=Url,KV).


%%% anstatŭigu linirompojn kaj citilojn por inkluzivi en JSON-mesaĝo kiel "String"
%%escape_data(Quoted,Escaped) :-
%%    replace_atom(Quoted,'"','\\"',Esc1),
%%    replace_atom(Esc1,'\n','\\n',Escaped).
%%
%%
%%replace_atom(In,From,To,Out) :-
%%    atom_codes(From,FromList),
%%    atom_codes(To,ToList),
%%    atom_codes(In,InList),
%%    replace(InList,FromList,ToList,OutList),
%%    atom_codes(Out,OutList).
%%
%%replace(InList,From,To,OutList) :-
%%    append(From,Rest,InList),!,
%%    replace(Rest,From,To,RestOut),
%%    append(To,RestOut,OutList).
%%
%%replace([X|RestIn],From,To,[X|RestOut]) :-
%%    replace(RestIn,From,To,RestOut).
%%
%%replace([],_,_,[]).