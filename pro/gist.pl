:- module(gist,[
    post_gist/5,
    get_gists/1
]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
%:- use_module(library(crypto)).

:- use_module(pro(cfg/agordo)).
:- use_module(pro(db/redaktantoj)).


post_gist(Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Quoted) :-
    agordo:get_config(github_token,AccessToken),
    atom_codes(Xml,Quoted),
    sigelo(Retadreso,Xml,Sigelo,_LSums), 
    info_json(Sigelo,InfoJson),
    atom_json_term(Info,InfoJson,[]),
    json_post_data(Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Info,Xml,JsonData),
    agordo:get_config(gh_gist_url,URL),
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
  get_gist_list(Retadreso,Gists),
  reply_json(Gists).

get_gist_list(Retadreso,Gists) :-
  agordo:get_config(github_token,AccessToken),
  agordo:get_config(gh_gist_url,URL), 
  atom_concat('token ',AccessToken,TokenHeader),
  http_get(URL, Reply, 
        [redirect(false),authenticate(false),request_header(authorization=TokenHeader)]),!,  
  email_redid(Retadreso,RedId),   
  sub_atom(RedId,0,7,_,RHash),     
  json_gist_list(RHash,Reply,Gists),!.


json_gist_list(_,[],[]).
json_gist_list(RHash,[json(Json)|Rest],[Gist|MoreGists]) :-
  % elfiltru gistojn de tiu ĉi redaktanto
  member(description=Desc,Json),
  atomic_list_concat([RHash|_],':',Desc),

  gist_list_item(Json,Gist),
  debug(gist,'  item: ~q~n',[Gist]),
  json_gist_list(RHash,Rest,MoreGists).

% ignoru aliajn, kiuj ne estas de tiu ĉi redaktanto
json_gist_list(RHash,[_|Rest],MoreGists) :-
  debug(gist,'  SKIP',[]),
  json_gist_list(RHash,Rest,MoreGists).


gist_list_item(Json,json([id=Id,desc=Desc2,created=Created,updated=Updated,
    name=XmlName,html_url=GistUrl,xml_url=XmlUrl,
    rezulto=Rezulto,rez_url=RezUrl])) :-
  member(id=Id,Json),
  member(description=Desc,Json),
  atomic_list_concat([_|D2],':',Desc),
  atomic_list_concat(D2,':',Desc2),
  member(created_at=Created,Json),
  member(updated_at=Updated,Json),
  member(html_url=GistUrl,Json),
  % nomo de la XML-dosiero
  member(files=json(FilesJson),Json),
  member(XmlName=json(JFile),FilesJson),
  member(type='application/xml',JFile),
  member(raw_url=XmlUrl,JFile),
  % nomo de la rezulto-dosiero, se estas
  once((
    member(files=json(FilesJson),Json),
    member(RName=json(RFile),FilesJson),
    member(RName,['konfirmo.json','eraro.json']),
    member(type='application/json',RFile),
    member(raw_url=RezUrl,RFile),
    sub_atom(RName,0,_,5,Rezulto)
    ;
    RezUrl='',
    Rezulto=''
  )).


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
json_post_data(Retadreso,Redakto,Dosiero,Shangho_au_Nomo,Info,Xml,
    json([description=Desc,files=Files])) :-        
    %escape_data(Info,InfoEsc),
    %escape_data(Xml,XmlEsc),
    email_redid(Retadreso,RedId),   
    sub_atom(RedId,0,7,_,RHsh),
    atomic_list_concat([RHsh,Redakto,Shangho_au_Nomo],':',Desc),
    atom_concat(Dosiero,'.xml',XmlDos),
    atom_concat(Dosiero,'.json',JsnDos),
    Files=json([
        XmlDos=json([content=Xml]),
        JsnDos=json([content=Info])
        ]).


info_json(Sigelo,json([sigelo=Sigelo,celo=Celo])) :- %,lsums=LSums])) :-
    agordo:get_config(github_repo,Repo),
    atom_concat(Repo,'/revo',Celo).
    
sigelo(Retadreso,Xml,Sigelo,'') :-
    agordo:get_config(sigelilo,Sigelilo),
    atomic_list_concat([Retadreso,Xml],'\n',Data),
    % debug...
    %line_sums(Data,LSums),
    hmac_sha(Sigelilo,Data,HMac,[algorithm(sha256)]),
    hash_atom(HMac,Sigelo).
    % this was running into a bug on Ubunut 16.04 VM, where Sigelilo
    % appearantly was not used:
    % crypto_data_hash(Data,Sigelo,[algorithm(sha256),hmac(Sigelilo)]).

line_sums(Xml,Sums) :-
  atomic_list_concat(Lines,'\n',Xml),
  line_sums_(Lines,S),
  atomic_list_concat(S,',',Sums).


line_sums_([],[]).
line_sums_([L|Lines],[S|Sums]):-
  atom_length(L,Len),
  sha_hash(L,[H1,H2|_],[]),
  hash_atom([H1,H2],H),
  format(atom(S),'~w-~w',[Len,H]),
  line_sums_(Lines,Sums).

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