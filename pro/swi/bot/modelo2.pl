/* -*- Mode: Prolog -*- */

:- use_module( library(chr)).
:- encoding(utf8).
:- discontiguous f_/2, s_/2.

/**
  La model baziĝas iom sur trajtoarboj (angle: "feature trees", ekz. vd. Smolka, CFT):

  f(X,F,Y) - X havas Y kiel trajton F, 
  s(X,S) - X estas de la speco S (angle: "sort")

  ekz. f(X,frukto,Y), s(Y,nukso)

  krome ni aldonas probablecon / pezon de ekzisto de trajto (sen, kun, ofte...):

  f(X,frukto,ofte:nukso),
  f(X,tigo,kun:dornoj),
  f(X,folioj,sen:haroj)

  kaj nombron

  f(F,stamenoj,(2-4))
  f(F,kaliko,sepaloj(4;5))

  ni permesas por trajtoj alternativojn (aŭ) aŭ konjunkcion (kaj).
  Decidenda: oni povus signi tion aŭ kiel subarbo: f(F,frukto,(nukso,drupo)) -> f(F,frukto,Kaj), f(Kaj,1,nukso), f(Kaj,2,drupo),
     same pri aŭ: (X;Y) -> f(Aŭ,1,X), f(Aŭ,2,Y) aŭ uzante la sintakson kiel probabelco supre:
     f(F,frukto,aŭ:nukso), f(F,frukto,aŭ:drupo)

  Malsupre estas la modelo, t.e. specifigo de eblaj trajtoj kaj specoj, ekz. la specoj de fruktoj kaj la trajtoj (partoj kaj ecoj ktp.) de fruktoj.
  ni uzas s_(frukto,nukso), s_(frukto,drupo)..., f_(frukto,fakoj,n_), ... n_(...) ellasante la unuan argumenton (signita per _)
   
  Kiam ni skribas difinojn pro koncizeco kaj evito de variabloj ni faldas (nestigas) la faktojn, aparte tio signifas:
  - ni povas forlasi la nomon de trajto, se la valoro implicas ĝin f(X,nukso) -> f(X,frukto,nukso)
  - ni uzas la nomon de speco rekte kiel valoro de trajto kaj ni nestigas (enkrampigas) sub-arbojn: f(X,nukso(verda)) -> f(X,frukto,F), s(F,nukso), f(F,koloro,verda)


   ... heredado, heredi trajtojn de pli ĝenerala speco (frukto -> nukso...)

**/



% modelo (radiko)
m_(planto).

f_(planto,grupo,grupo).
s_(grupo,familio).
s_(grupo,genro).

f_(planto,tipo). % mlg. f_(planto,tipo,tipo), s(tipo, daŭrotipo) ktp.
s_(tipo,daŭrotipo).
s_(daŭrotipo,lignoplanto).

s_(planto,lignoplanto). % ĉu problema ambigueco en la modelo: lignoplanto estas plantospeco kaj ankaŭ daŭrotipo...?
s_(lignoplanto,arbo).
s_(lignoplanto,arbedo).

f_(planto,alteco).
s_(alteco,alta). % valoroj estas esprimataj per specoj, ĉu alternative uzi apartan v_(alteco,alta). ?
s_(alteco,malalta).

f_(alteco,longeco).

f_(planto,tigoj).
s_(tigoj,parto).
f_(tigoj,dornoj).
f_(tigoj,dornaj). % "dornaj" estas trajto de tigoj, ne de folioj ktp.
s_(dorna,eco).

s_(eco,tre).
s_(eco,globaj).
f_(parto,koloro).
s_(koloro,flava).
s_(koloro,oranĝruĝa).

f_(planto,kreskanta_ĉe).
s_(kreskanta_ĉe,bordoj).

longeco(L) :- atom_concat(N,cm,L), number(N), !.
longeco(L) :- atom_concat(N,m,L), number(N).


    /*
vivejo: akvoplanto

daŭro
- daŭra (neligna: staŭdo; ligna: arbedo, arbusto, arbo)
- nedaŭra (herbo unujara: vintra, somera; herbo dujara; herbo plurjara)

radiko:
- alorizo: pivoto (beto), homorizo; bulbo, tubero, rizomo

floro:
- flortigo (pedunklo, pedicelo)
- florfundo
#- florumo: korolo+kaliko
    korolo: petaloj
    kaliko: sepaloj
    ambaŭ aŭ nedistingeble: tepaloj
- pistilo (ina): ovario, stiluso, stigmo
- malina: stameno

frukto:
- formo: fermfrukto: nukso, bero, fendofrukto, drupo
      malfermfrukto: foliklo, silikvoj, kapsulo, guŝo
- partoj:
   "fruktonodo" = ovolujo/ovario...

*/

% s(X,S): speco (tipo) de X estas/povas esti S
% f(P,T,S): trajto de parto P estas S (valoro aŭ speco...)
:- chr_constraint s/2, f/2, f/3.


% folio(kruce(alternaj),ofte(kun(kromfolio)))).
% => folioj( pozicio:kruce_alternaj , kromfolioj:ofte )
% implice: => folioj(kruce_alternaj, kromfolioj(ofte) ).


malfaldo(G) :-
    genro(G,_Trd,_Tipo,Trajtoj),
    maplist(trajto(G),Trajtoj).

trajto(Planto,T) :-
%    Trajto =.. [T|Args],
    f(Planto,T).

malfaldo @ f(X,Y) <=> atom(Y), f_(F,Y) | f(X,F,Y).
malfaldo @ f(X,Y) <=> Y=..[S|_], f_(F,S) | f(X,F,Y).
malfaldo @ f(X,F,Y) <=> compound(Y) |  Y=..[S|Y1], f(X,F,S1), s(S1,S), f(S1,Y1).

%malfaldo(f(X,Y),f(X,F,Y) :- f_(F,Y).
%malfaldo(f(X,F,Y), ) :- s_(_,Y)...



genroj(eleagn, [elagn,hipofe]).

genro(hipofe,
        [ la('Hippophae'), de('Sanddorn') ],
        [ (arbedo;arbo(alta(ĝis:'6m'))) ],
        [
            dornaj(tre), % samsignife: f(X,tigoj,kun:dornoj(multaj))
            floraroj(globaj),
            frukto((oranĝruĝa;malofte:flava)),
            kreskanta_ĉe(bordoj)
        ]).

