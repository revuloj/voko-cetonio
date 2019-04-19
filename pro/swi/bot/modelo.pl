/* -*- Mode: Prolog -*- */

%:- use_module( library(chr)).
:- encoding(utf8).
:- discontiguous f/3, s/1, s/2.

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
**/
		    
    /*
vivejo: akvoplanto

daŭro:
- daŭra (neligna: staŭdo; ligna: arbedo, arbusto, arbo)
- nedaŭra (herbo unujara: vintra, somera; herbo dujara; herbo plurjara)

radiko:
- alorizo: pivoto (beto), homorizo; bulbo, tubero, rizomo

floro:
- flortigo (pedunklo, pedicelo)
- florfundo
- florumo: korolo+kaliko
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
%:- chr_constraint s/2, f/3.


% folio(kruce(alternaj),ofte(kun(kromfolio)))).
% => folioj( pozicio:kruce_alternaj , kromfolioj:ofte )
% implice: => folioj(kruce_alternaj, kromfolioj(ofte) ).


/*************************************************************
*  TIPOJ
*************************************************************/

% bazaj specoj estas planto, grupo, tipo

s(planto).
s(grupo).
s(tipo).

% grupoj de plantoj por klasifikado
s(grupo,specio).
s(grupo,genro).
s(grupo,familio).

% grupoj laŭ vivkondiĉoj
s(tipo,daŭrotipo).
s(tipo,geofito). % rekreskaj burĝonoj sub tero dum seka aŭ frosta periodo
s(tipo,hidrofito). % rekreskaj burĝonoj sub akvo dum malfavora periodo
s(tipo,helofito). % rekreskaj burĝonoj en marĉo dum malfavora periodo
% tipoj laŭ vivejo
s(tipo,akvoplanto).
s(tipo,liano).

% tipoj laŭ daŭroformoj
s(daŭrotipo,herbo). % unu, dujaraj
s(daŭrotipo,staŭdo). % plurjaraj -> geofito
s(daŭrotipo,lignoplanto).
s(lignoplanto,arbo).
s(lignoplanto,arbedo).
s(lignoplanto,arbusto).
s(lignoplanto,duonarbusto).

% sekstipoj
%f(planto,sekseco,sekseco_).
s(tipo,sekstipo).
s(sekstipo,monoika). % unuseksaj floroj sur diversaj individuoj
s(sekstipo,dioika).  % unu (aŭ duseksaj?) floroj sur sama individuo

% tipoj povas havi ecojn
f(tipo,eco,ecoj_).

% genroj, unu..n
f(familio,genroj,nombro_).
% s(N,nombro).
s(nombro_,N) :- number(N); member(N,[sola,unu,du,tri,kvar,kvin,ses,sep,ok,naŭ,dek]).
s(nombro_,tio_ĉi_ne_estas_nombro). % truko por kompletigi la modelon, alternative traktu nombron aparte en modelkontrolo!


/*************************************************************
*  TRAJTOJ
*
*  trajtoj estas partoj de plantoj, kiuj aŭ mem jam pli priskribas ĝin,
*  ekz. "rizomo" estas aparta speco de radiko, aŭ per ecoj estas pli
*  specifataj, ekz. folioj(glatrandaj), stamenoj(4-6,...) ktp.
*
* PLIBONIGU:
* difinu ĉiujn partojn en listo partoj[X,Y,Z,...] aŭ kiel parto(X). parto(Y)...
* uzu term_expansion por difini predikatojn X(), Xj(), X(Ecoj), Xj(Ecoj
*************************************************************/

% partoj/trajtoj de plantoj (kaj ties tipoj)
f(planto,radiko,radiko_speco_).  % f(planto,radiko,_r), s(_r,rizomo) ?
f(planto,tigo,tigo_speco_).
f(planto,folio,folio).
f(planto,floraro,floraro_).
f(planto,floro,floroparto_).
f(planto,frukto,frukto_).

% tipoj de radiko
s(radiko_speco_,rizomo).
s(radiko_speco_,tubero).
s(radiko_speco_,pivotradiko).

% tipoj de tigo
s(tigo_speco_,tigo).
s(tigo_speco_,trunko).
s(tigo_speco_,ŝoso).

% partoj/trajtoj de tigo
f(tigo,ligno,ecoj_).
f(trunko,ŝelo,ecoj_).

% partoj/trajtoj de folio
%s(folio_ecoj_,folio). % alternative difinu pozicio, kromfolio ktp. kiel specoj de folio(sp)ecoj_ ?
s(folio).
f(folio,pozicio,pozicioj_).
f(folio,kromfolio,ecoj_).  % ecoj: jes, ne, nombro, grandeco
%f(folio,akselo,ecoj_). % pli rolas por signi pozicion de aliaj partoj, ekz. foloroj en foliakseloj
f(folio,surfaco,ecoj_).
f(folio,rando,ecoj_). % PLIBONIGU: listigu randojn laŭeble s(folirandoj_,denta)...
f(folio,formo,ecoj_). % PLIBONIGU: listigu formojn laŭeble...
f(folio,lobo,ecoj_). % ecoj: jes, ne, nombro, grandeco, ofteco

% pozicioj de folioj
s(pozicioj_,alternaj).
s(pozicioj_,kontraŭaj).
s(pozicioj_,bazaj).
s(pozicioj_,rozeto).
s(pozicioj_,ĉirkaŭaj).

% floraroj
s(floraro_,grapolo). % Traube
s(floraro_,amento). % Kätzchen
s(floraro_,tirso). % Thyrse
s(floraro_,umbelo). % Dolde
s(floraro_,cumo). % Zyme
s(floraro_,paniklo). % Ähre
s(floraro_,korimbo). % Schirmtraube
s(floraro_,korimbaro).
% ...
	      
% tipoj de floro
s(floro,vira_floro). % au f(floro,sekso,sekso(S)). s(sekso,vira). s(sekso,virina). % mll.: f(floro,sekso,(vira;virina))
s(floro,virina_floro).

% partoj de floro
s(floroparto_,floro). % ???
s(floroparto_,pistilo). % ???
s(floroparto_,florumo). % ???
s(floroparto_,ovolujo).
f(floro,akso,ecoj_).
f(floro,florumo,ecoj_). % duobla: sepaloj+petaloj, unuobla: tepaloj
f(floro,tepalo,ecoj_). % sepalo kaj/aŭ petalo, aparte se ne distingeblaj
f(floro,sepalo,ecoj_). % kune formas kalikon
f(floro,petalo,ecoj_). % kune formas korolon

f(floro,stameno,ecoj_).
f(floro,pistilo,ecoj_). % + eco(E).
f(pistilo,stiluso,ecoj_).
f(pistilo,stigmo,ecoj_).
f(pistilo,ovolujo,ecoj_).

f(florumo,kaliko,ecoj_).
f(florumo,korolo,ecoj_).
f(floro,fundo,ecoj_). % aŭ fakte parto de florumo/kaliko ..? % = hipantio / kaliko ... Achsenbecher, Blütenachse, Blütenkelch
f(ovolujo,fako,nombro_).

% tipoj de frukto
s(frukto_,kapsulo).
s(frukto_,bero).
s(frukto_,drupo).
s(frukto_,silikvo). % duĉambra kapsulo
s(frukto_,nukso).
s(frukto_,nukseto).
s(frukto_,grupfrukto).
s(frukto_,plurfrukto). % ĉu la sama kiel plurfrukto?
s(frukto_,foliklo). % fendfrukto, unuvalva, unuĉambra kapsulo, malfermiĝanta nur en la ventra suturo. (Balgfrucht)
s(frukto_,guŝo). % unufaka malfermfrukto

% aliaj trajtoj / partoj de plantoj
% necesus iel specifi, ĉu ili povas esti trajtoj de ĉiuj plantpartoj aŭ nur certaj
f(planto,suko,ecoj_).
%f(frukto,fako,ecoj_). % ĉu ĉiuj fruktoj povas havi fakojn? -> kapsulo, silikvo...
%f(frukto,fako,nombro_). % ĉu trakti nombron kiel specialan econ?
f(planto,harfasko,ecoj_).
f(planto,elstaraĵo,ecoj_).
f(planto,kovro,ecoj_).

% ecoj...
%s(ecoj_,E) :- atom(E). % simpla valoro, ekz. 'granda'
%s(ecoj_,N) :- s(nombro_,N). % ĉu trakti nombrojn kiel aparta eco aŭ kiel aparta speco: eco(nombro) resp. nombro(N)
s(ecoj_,Eco) :- modif(Eco). % modifiloj -> vd. malsupre
s(ecoj_,konj).
s(ecoj_,disj).

/*************************************************************
*  MODIFILOJ de ecoj
*
*  modifiloj estas uzataj por pli bone priskribi ecojn de plantopartoj/trajtoj
*  fakte oni ne bezonas ilin specifi se uzataj kiel "atom", nur si ilin enhvas pliajn ecoj, ekz. arome(odoranta)
*  povus esti ideo ankaŭ specifi la atomajn ecojn por kontroli ilin aŭ scii, kiuj entute eblas...
************************************************************/

modif(Eco) :- prep(Eco);adv(Eco);adj(Eco).
prep(Prep) :- memberchk(Prep,[po,en,de,ĉe,por,pro,ĝis,je,inter,super,sub,kun,sen]).
adv(Adv) :- memberchk(Adv,[ofte,malofte,tre,foje,false,frue,duoble,apenaŭ,egale,neegale,kruce,rozete,nepare,arome,klape,helice,okulfrape,travideble,fine,forte]).
adj(Adj) :- senplural(Adj,A), memberchk(A,[uzata,ĉirkaŭita,simila,makulita,defalanta,punktita,kunkreskinta,kreskanta,disfalanta,karnovora,unujara,dujara,plurjara,somera,vintra]).

% ankoraŭ konsideru aparte:
%pro_tio(Eco)
%sola(Eco)
%ĉirkaŭita_de(Eco)
%simila_al(Eco)
%fine_de(Eco)
% kreskanta_ĉe(Eco)
% kapti(Eco)
% disfalantaj(Eco)
% alta(Eco) % ĉu: pli alta ol, alta ĝis...?

% areo (de...ĝis)
% '-'(A1,A2) --> { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " ĝis ", C2.

% konjunkcio kaj disjunkcio
%kaj([A1,A2]) --> A1, " kaj", A2. % { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " kaj ", C2.
%kaj(A1,A2) --> A1, " kaj ", A2. % { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " kaj ", C2.
%aŭ(A1,A2) --> { A2 = (A3;A4), ! }, A1, ", ", aŭ(A3,A4).
%aŭ(A1,A2) --> A1, " aŭ ", A2. % { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " aŭ ", C2.


kontrolu_modelon :- forall(kontrolu_m,true).
kontrolu_familion(Fam) :-
    familio(Fam,_Trd,Tipj,Trajtj),
    kontrolu_trajtojn(Tipj),
    kontrolu_trajtojn(Trajtj).

kontrolu_m :-
    % ĉiu trajto (S,F,_) estu ligita la speco, kiu estas deklarita per s(S) aŭ kiel subspeco s(_,S)
    f(S,F,X),
    (
	( s(S) ; clause(s(_,S),true) ) -> true
	;
	throw(speco_ne_difinita(S,f(S,F,X)))
    ).

kontrolu_m :-
    % ĉiu speco-variablo atribuata al trajto estu difinita(? -> eble permesu ĝin aliokaze kiel simpla valoro)
    f(X,F,S),
    (
	( s(S) ; clause(s(S,_),true) ) -> true
        ;
	throw(speco_ne_difinita(S,f(X,F,S)))
    ).


kontrolu_m :-
    % ĉiu speco estu baza tipo aŭ subspeco de alia aŭ estu uzata en trajto
    s(S,X),
    (
	( s(S) ; clause(s(_,S),true) ; f(_,_,S) ) -> true
	;
	throw(speco_ne_uzata(S,s(S,X)))
    ).

kontrolu_m :- % se venas ĝis tie ĉi, ĉio estas bone...
    true, !.


kontrolu_trajtojn(Trajtoj) :-
    maplist(kongrua_dbg,Trajtoj).

kongrua_dbg(Trajt) :-
    debug(trajto,'~q',[Trajt]),
    kongrua(Regulo,Trajt), !, % ĉu permesi plurajn eblecojn de kongruo?
    debug(kongrua,'--> ~q: ~q',[Regulo,Trajt]).

% => folioj( pozicio:kruce_alternaj , kromfolioj:ofte )
% implice: => folioj(kruce_alternaj, kromfolioj(ofte) ).

speco(S) :- s(S) ; s(_,S).
super_sub(S,S).
super_sub(Super,Sub) :- s(Super,Sub) ; s(S,Sub), super_sub(Super,S).
senplural(EbleKun,Sen) :- atom(EbleKun), once(( atom_concat(Sen,j,EbleKun) ; Sen = EbleKun)).

% trajto povas esti simpla speco (tipo)
kongrua(speco,Tj) :-
    senplural(Tj,T),
    speco(T).

/* setu f(T,eco,ecoj_). anstataŭe...
% trajto povas esti speco kun eco, ekz staŭdo(karnovora)
kongrua(T) :-
    T =.. (S,E),
    speco(S),
    s(ecoj_,E).
*/

% povas esti speco kun implicitaj ecoj
% ekz. herbo(ofte)
kongrua(speco_eco,T) :-
    T =.. [Sj,Eco],
    senplural(Sj,S),
    speco(S),
    f(Super,eco,E),
    super_sub(Super,S),
    s(E,Eco).

% povas esti trajto kun speco en krampoj,
% ekz. folio(pozicio(alternaj)) <=> folio [ pozicio ] alternaj <=> f(folio,pozicio,alternaj)
kongrua(trajto_ekspl,T) :-
    T =.. [F,Spc],
% F estas parto/trajto de io
    f(_,F,S),
    Spc =.. [F1,Spc2],
% F1 estas trajto de F kaj havas specon Spc2
    f(S,F1,S1), s(S1,Spc2).


% trajtonomon oni povas ellasi, se la speco klare difinas ĝin:
% folio(pozicio(alternaj)) <=> folio [ _ ] alternaj
% FARENDA: tiun unikecon speco -> trajto oni devus ankoraŭ certigi aŭ
% en la modelkontrolo aŭ tie ĉi!
kongrua(trajto_impl,T) :-
    T =.. [F,Spc],
% F estas parto/trajto de io
    ( f(_,F,S) ; atom_concat(F_,j,F), f(_,F_,S) ),
% F1 estas trajto de F kaj havas specon Spc2
    f(S,_F1,S1), clause(s(S1,Spc),true).


speco_kongrua(S,S) :-
%    senplural(Sj,S),
    speco(S).

speco_kongrua(S,S1) :-
    S1 =.. [S,Eco],
%    senplural(Sj,S),
    speco(S),
    f(Super,eco,E),
    super_sub(Super,S),
    s(E,Eco).


/*
% S: nomo de eblaj specoj el f(_,_,S), S1 la esprimo, por eltrovi ĉu konvenas al S
kongrua(S,S1) :-
    s(S,S1).
*/
     % povas esti konj / disj / modif
     % ...


