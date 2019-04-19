%:- use_module( library(chr)).
:- encoding(utf8).

:- consult(familioj).
:- consult(genroj).

%:- chr_constraint la/1, de/1.

%ek, d(D) <=> upcase_atom(D,U), d(U).
%d(A), d(B) <=> atom_concat(A,B,C), d(C).

%rizomo(Adj) :- atomic(Adj), ĉeno(rizomo,Adj).
%rizomo(Ecoj) :- is_list(Ecoj), ĉeno(Ecoj), ĉeno(rizomo).
%rizomo(Ecoj) :- compound(Ecoj), ĉeno(Ecoj), ĉeno(rizomo).

%kaj(X,Y).
%aŭ(X,Y).

%fam_dif(Fam) :- familio(Fam,EcListo),
%		maplist(call,EcListo).


ref(floroplant,'flor.0oplant').
ref(famili,'famili.0o.BIO').
ref(genr,'genr.0o.BIO').

ordo(Fam,Ord) :- familioj(Ord,L), member(Fam,L), !.
ordo(Fam,_) :- throw(familio_sen_ordo(Fam)).

familio(Gnr,Fam) :- genroj(Fam,L), member(Gnr,L), !.
familio(Gnr,_) :- throw(genro_sen_familio(Gnr)).

familio(Fam) :- familio(Fam,Trd,Tipoj,Trajtoj),
		% maplist(traduko,Tradukoj),
		maplist(tipo,Tipoj,Tpj),
		maplist(trajto,Trajtoj,Trj),
		write_fam_dif(Fam,Tpj,Trj,Trd).

genro(Gnr) :- genro(Gnr,Trd,Tipoj,Trajtoj),
		maplist(tipo,Tipoj,Tpj),
		maplist(trajto,Trajtoj,Trj),
		write_gnr_dif(Gnr,Tpj,Trj,Trd).

write_fam_dif(Fam,Tipoj,Trajtoj,Tradukoj) :-
    ref(famili,Ref),
    write_lst('Familio',Ref,botanikaj_familioj),
    write('de '),
    atomic_list_concat(Tipoj,', ',Tpj),
    writeln(Tpj),
    write_ordo(Fam,floroplant),
    memberchk(la(Lat),Tradukoj) ->
      (write(' ('), write_trd(la,Lat), write(')')),
    writeln(' kun:'),
    atomic_list_concat(Trajtoj,';\n',Trj),
    write(Trj),
    writeln('.').


write_gnr_dif(Gnr,Tipoj,Trajtoj,Tradukoj) :-
    ref(genr,Ref),
    atomic_list_concat(Tipoj,', ',Tpj),
    writeln(Tpj),
    writeln(' kun:'),
    atomic_list_concat(Trajtoj,';\n',Trj),
    write(Trj),
    writeln(';'),
    write_lst('genro',Ref,botanikaj_genroj),
    (memberchk(la(Lat),Tradukoj) ->
	(write('('), write_trd(la,Lat), write(') '))),
    write_familio(Gnr),
    writeln('.').
    
write_ordo(Fam,Grp) :-
    ordo(Fam,Ord),
    once((
	ref(Grp,Ref),
	format('el la ordo de ~woparencaj~n',[Ord]),
%	atom_concat(Ord,'oparencoj',Lst),
	atom_concat(Grp,'oj',Nom),
	write_ref(Nom,Ref,malprt)
	;
	format('el la ordo de ~woparencaj ~woj~n',[Ord,Grp])
	)).

write_familio(Gnr) :-
    familio(Gnr,Fam),
    write('el la familio '),
    atom_concat(Fam,'acoj',Nom),
    atom_concat(Fam,'ac.0oj',Ref),
    write_ref(Nom,Ref,malprt),
    write(',\nordo de '),
    ordo(Fam,Ord),
    atom_concat(Ord,'oparencoj',Lst),
    write_lst(Lst,Ref,Lst).

write_trd(Lng,Trd) :-
    format('<trd lng="~w">~w</trd>',[Lng,Trd]).

write_lst(Nom,Ref,Lst) :-
    format('<ref tip="lst" lst="voko:~w" cel="~w">~w</ref>~n',[Lst,Ref,Nom]).

write_ref(Nom,Ref,Tip) :-
    format('<ref tip="~w" cel="~w">~w</ref>',[Tip,Ref,Nom]).

traduko(Trd) :-
    Trd =.. [T|Args],
    memberchk(T,[la,de]),
    call(T,Args).

tipo(Tipo,Vortoj) :-
    Tipo =.. [T|Args],
    once((
 		% neniu argumento
		Args = [],
		phrase(Tipo,V)
		;      
	        % unu argumento
	        Args = [_],
	        phrase(Tipo,V)
		;
		% disjunkcio     
		T = ';',
		Args = [A1,A2],
		phrase(aŭ(A1,A2),V)
		;
		% konjunkcio
		T = ',',
		Args = [A1,A2],
		phrase(kaj(A1,A2),V)
		;
		% pluraj argumentoj
		T1 =.. [T,Args],
		phrase(T1,V)
   )),
   atom_codes(Vortoj,V).    

%trajto(T) :- 
trajto(Trajto,Vortoj) :-
    Trajto =.. [T|Args],
    once((     
	         % unu argumento
	         Args = [_],
	         phrase(Trajto,V)
		 ;
		 % disjunkcio     
		 T = ';',
		 Args = [A1,A2],
		 phrase(aŭ(A1,A2),V)
		 ;
		 % konjunkcio
		 T = ',',
		 Args = [A1,A2],
		 phrase(kaj(A1,A2),V)
		 ;
		 % pluraj argumentoj
		 T1 =.. [T,Args],
		 phrase(T1,V)
   )),
   atom_codes(Vortoj,V).    

% transformo de predikato kun pli ol unu argumento al
% predikato super listo
arg_listo(CompoundN,Compound1) :-
    CompoundN =.. [T|Args],
    once((     
	     % unu argumento
	     (Args = [] ; Args = [_]),
	     Compound1 = CompoundN
	     ;
	     % disjunkcio     
	     T = ';',
	     Args = [A1,A2],
	     Compound1 = aŭ(A1,A2)
	     ;
	     % konjunkcio
	     T = ',',
	     Args = [A1,A2],
	     Compound1 = kaj(A1,A2)
	     ;
	     % pluraj argumentoj
	     Compound1 =.. [T,Args]
   )).
	

/*
ĉeno(X,Adj) :- atomic_list_concat([X,Adj],' ',D), d(D).

ĉeno([]) :- !.
ĉeno(X) :- atomic(X), d(X), !.
ĉeno([H|T]) :- ĉeno(H), ĉeno(T).
*/

/*************************************************************
*  TIPOJ
*************************************************************/

herboj --> "herboj".
herboj(Ecoj) --> "herboj ",ecoj(Ecoj).
staŭdoj --> "staŭdoj".
staŭdoj(Ecoj) --> "staŭdoj ",ecoj(Ecoj).

geofitoj --> "geofitoj".
akvoplantoj --> "akvoplantoj".
lianoj --> "lianoj".

lignoplantoj --> "lignoplantoj".
lignoplantoj(Ecoj) --> "lignoplantoj ",ecoj(Ecoj).

arbo --> "arbo".
arboj --> "arboj".
arbo(Ecoj) --> "arbo ",ecoj(Ecoj).
arboj(Ecoj) --> "arboj ",ecoj(Ecoj).

arbedoj --> "arbedoj".
arbedo --> "arbedo".
arbedoj(Ecoj) --> "arbedoj ",ecoj(Ecoj).

arbustoj --> "arbustoj".
duonarbustoj --> "duonarbustoj".

monoika([]) --> " monoika".
dioika([]) --> " dioika".
dioikaj --> "dioikaj".
monoikaj --> "monoikaj".

% genroj
genro(Ecoj) --> "genro ", ecoj(Ecoj).

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


% radikoj
rizomo --> "rizomo".
rizomo(Ecoj) --> parto_ecoj("rizomo",Ecoj).

% tigoj
tigoj(Ecoj) --> parto_ecoj("tigoj",Ecoj).
tigo(Ecoj) --> parto_ecoj("tigo",Ecoj).
trunko(Ecoj) --> parto_ecoj("trunko",Ecoj).
trunkoŝelo(Ecoj) --> parto_ecoj("trunkoŝelo",Ecoj).
ligno(Ecoj) --> parto_ecoj("ligno",Ecoj).
ŝosoj(Ecoj) --> parto_ecoj("ŝosoj",Ecoj).

% folioj
folioj(Ecoj) --> parto_ecoj("folioj",Ecoj).
kromfolioj(Ecoj) --> parto_ecoj("kromfolioj",Ecoj).
akseloj(Ecoj) --> parto_ecoj("akseloj",Ecoj).
loboj(Ecoj) --> parto_ecoj("loboj",Ecoj).

% floraroj
grapoloj(Ecoj) --> parto_ecoj("grapoloj",Ecoj).
amentoj(Ecoj) --> parto_ecoj("amentoj",Ecoj).
tirsoj(Ecoj) --> parto_ecoj("tirsoj",Ecoj).
tirsoj --> "tirsoj".
umbeloj(Ecoj) --> parto_ecoj("umbeloj",Ecoj).
cumoj(Ecoj) --> parto_ecoj("cumoj",Ecoj).
cumoj --> "cumoj".
paniklo --> "paniklo".
korimbaro --> "korimbaro".
	      
%floroj
floro(Ecoj) --> parto_ecoj("floro",Ecoj).
floroj(Ecoj) --> parto_ecoj("floroj",Ecoj).
floraroj(Ecoj) --> parto_ecoj("floraroj",Ecoj).
viraj_floroj(Ecoj) --> parto_ecoj("viraj floroj",Ecoj).
virinaj_floroj(Ecoj) --> parto_ecoj("virinaj floroj",Ecoj).
florakso(Ecoj) --> parto_ecoj("florakso",Ecoj).

florumo(Ecoj) --> parto_ecoj("florumo",Ecoj). % duobla: sepaloj+petaloj, unuobla: tepaloj
sepaloj(Ecoj) --> parto_ecoj("sepaloj",Ecoj).
tepaloj(Ecoj) --> parto_ecoj("tepaloj",Ecoj).
petaloj(Ecoj) --> parto_ecoj("petaloj",Ecoj).

stamenoj(Ecoj) --> parto_ecoj("stamenoj",Ecoj).
ovolujo(Ecoj) --> parto_ecoj("ovolujo",Ecoj).
ovolujoj(Ecoj) --> parto_ecoj("ovolujoj",Ecoj).
stigmo(Ecoj) --> parto_ecoj("stigmo",Ecoj).
pistiloj(Ecoj) --> parto_ecoj("pistiloj",Ecoj).
pistilo(Ecoj) --> parto_ecoj("pistilo",Ecoj).
stilusoj(Ecoj) --> parto_ecoj("stilusoj",Ecoj).

kaliko(Ecoj) --> parto_ecoj("kaliko",Ecoj).
fundo(Ecoj) --> parto_ecoj("fundo",Ecoj). % = hipantio / kaliko
florfundo(Ecoj) --> parto_ecoj("florfundo",Ecoj). % ... Achsenbecher, Blütenachse, Blütenkelch

% fruktoj
frukto(Ecoj) --> parto_ecoj("frukto",Ecoj).
fruktoj(Ecoj) --> parto_ecoj("fruktoj",Ecoj).
kapsulo(Ecoj) --> parto_ecoj("kapsulo",Ecoj).
kapsuloj(Ecoj) --> parto_ecoj("kapsuloj",Ecoj).
kapsuloj --> "kapsuloj".
beroj(Ecoj) --> parto_ecoj("beroj",Ecoj).
drupoj --> "drupoj".
drupoj(Ecoj) --> parto_ecoj("drupoj",Ecoj).
silikvo(Ecoj) --> parto_ecoj("silikvo",Ecoj).
nukso --> "nukso".
nukso(Ecoj) --> parto_ecoj("nukso",Ecoj).
nukseto --> "nukseto".
nukseto(Ecoj) --> parto_ecoj("nukseto",Ecoj).
nuksetoj(Ecoj) --> parto_ecoj("nuksetoj",Ecoj).
nuksoj(Ecoj) --> parto_ecoj("nuksoj",Ecoj).
nuksoj --> "nuksoj".
grupfruktoj(Ecoj) --> parto_ecoj("grupfruktoj",Ecoj).
plurfruktoj --> "plurfruktoj".
folikloj(Ecoj) --> parto_ecoj("folikloj",Ecoj).
semoj(Ecoj) --> parto_ecoj("semoj",Ecoj).
guŝoj(Ecoj) --> parto_ecoj("guŝoj",Ecoj).

% aliaj trajtoj / partoj
suko(Ecoj) --> ecoj(Ecoj), " suko".
fakoj(Ecoj) --> ecoj(Ecoj), " fakoj".
harfaskoj(Ecoj) --> parto_ecoj("harfaskoj",Ecoj).
elstaraĵoj(Ecoj) --> parto_ecoj("elstaraĵoj",Ecoj).
kovro(Ecoj) --> parto_ecoj("kovro",Ecoj).




% se unua eco estas nombro, metu antaŭe,
% aliajn ecojn metu malantaŭe
parto_ecoj(Parto,Eco) --> { atom(Eco), ! }, nombro(Eco,E), Parto, eble_spaco(E), ecoj(E). 
parto_ecoj(Parto,Ecoj) --> { is_list(Ecoj), ! }, nombro(Ecoj,E), Parto, eble_spaco(E), ecoj(E).
parto_ecoj(Parto,(E1;E2)) --> !, Parto, " ", ecoj(E1;E2).
parto_ecoj(Parto,Ecoj) --> { Ecoj =.. [E|Args], length(Args,L), L>1, !, E =.. [E,Args] }, Parto, " ", ecoj(E).
parto_ecoj(Parto,Ecoj) --> nombro(Ecoj,E), Parto, eble_spaco(E), ecoj(E).

% prenas nombojn de la komenco
nombro([N1-N2|R],R) --> N1-N2, !, " ".
nombro(kunkreskintaj(Eco) --> parto_ecoj("kunkreskintaj",Eco).N1-N2,[]) --> N1-N2, !, " ".
nombro(N,[]) --> { number(N), atom_codes(N,C) }, C, " ".
nombro([N|R],R) --> { number(N), atom_codes(N,C) }, C, " ".
nombro(N,[]) --> { memberchk(N,[unu,du,tri,kvar,kvin,ses,sep,ok,naŭ,dek,multaj]), atom_codes(N,C) }, C, " ".
nombro([N|R],R) --> { memberchk(N,[unu,du,tri,kvar,kvin,ses,sep,ok,naŭ,dek,multaj]), atom_codes(N,C) }, C, " ".
nombro(R,R) --> [].

eble_spaco([]) --> [], !.
eble_spaco(_) --> " ".


% alternativoj (aŭ)
ecoj([(E1;E2)]) --> ecoj(E1;E2).
ecoj((E1;E2)) --> { atom(E1), atom(E2), !, atom_codes(E1,C1), atom_codes(E2,C2) }, C1, " aŭ ", C2.
ecoj((E1;E2)) --> { atom(E1), atom_codes(E1,C1), E2 = (_;_), ! }, C1, ", ", ecoj(E2).
ecoj((E1;E2)) --> { atom(E1), atom_codes(E1,C1), ! }, C1, " aŭ ", ecoj(E2).
ecoj((E1;E2)) --> { compound(E1) }, ecoj(E1), " aŭ ", ecoj(E2).

% unuopa eco, pluraj ecoj (konjunkcio)
ecoj([E]) --> { atom(E), !, atom_codes(E,C) }, C.
% ecoj([E]) --> { compound(E), !}, E.
ecoj([E]) --> { E =.. [E1|Args], length(Args,L), L>1, !, E2 =.. [E1,Args]}, E2.

ecoj([E|R]) --> { E = (_;_) }, ecoj(E), ", ", ecoj(R).
ecoj([E|R]) --> { compound(E), arg_listo(E,E1), ! }, E1, ", ", ecoj(R).
ecoj([E|R]) --> { atom(E), atom_codes(E,C) }, C, ", ", ecoj(R).

ecoj(E) --> { atom(E), !, atom_codes(E,C) }, C.
ecoj(E) --> { E = (E1,E2), ! }, ecoj(E1), ", ", ecoj(E2).
ecoj(E) --> { compound(E), ! }, E.

ecoj([]) --> [].

/*************************************************************
*  MODIFILOJ de ecoj
*
*  modifiloj estas uzataj por pli bone priskribi ecojn de plantopartoj/trajtoj
************************************************************/

po(Eco) -->   prep_eco("po",Eco).
en(Eco) -->  prep_eco("en",Eco).
de(Eco) -->  prep_eco("de",Eco).
ĉe(Eco) -->  prep_eco("ĉe",Eco).
por(Eco) -->  prep_eco("por",Eco).
pro(Eco) -->  prep_eco("pro",Eco).
ĝis(Eco) -->  prep_eco("ĝis",Eco).
inter(Eco) -->  prep_eco("inter",Eco).
je(Eco) -->  prep_eco("je",Eco).
super(Eco) -->  prep_eco("super",Eco).
pro_tio(Eco) -->  prep_eco("pro tio",Eco).
sen(Eco) -->  prep_eco("sen",Eco).
ofte(Eco) -->  prep_eco("ofte",Eco).
tre(Eco) -->  prep_eco("tre",Eco).
malofte(Eco) -->  prep_eco("malofte",Eco).
foje(Eco) -->  prep_eco("foje",Eco).
false(Eco) -->  prep_eco("false",Eco).
frue(Eco) -->  prep_eco("frue",Eco).
duoble(Eco) -->  prep_eco("duoble multaj kiel",Eco).
kun(Eco) --> prep_eco("kun",Eco).
apenaŭ(Eco) --> prep_eco("apenaŭ",Eco).
neegale(Eco) -->  prep_eco("neegale",Eco).
sola(Eco) --> prep_eco("sola",Eco).
uzataj(Eco) --> prep_eco("uzataj",Eco).
ĉirkaŭita_de(Eco) --> prep_eco("ĉirkaŭita de",Eco).
simila_al(Eco) --> prep_eco("simila al",Eco).
fine_de(Eco) --> prep_eco("fine de",Eco).
kruce(Eco) -->  prep_eco("kruce",Eco).
rozete(Eco) -->  prep_eco("rozete",Eco).
nepare(Eco) -->  prep_eco("nepare",Eco).
arome(Eco) -->  prep_eco("arome",Eco).
klape(Eco) -->  prep_eco("klape",Eco).
helice(Eco) -->  prep_eco("helice",Eco).
okulfrape(Eco) -->  prep_eco("okulfrape",Eco).
travideble(Eco) -->  prep_eco("travideble",Eco).
forte(Eco) -->  prep_eco("forte",Eco).
etendita(Eco) -->  prep_eco("etendita",Eco).
defalanta(Ecoj) --> prep_eco("defalanta ",Ecoj).
makulita(Ecoj) --> prep_eco("makulita",Ecoj).
punktitaj(Ecoj) --> prep_eco("punktitaj",Ecoj).
kunkreskintaj(Eco) --> prep_eco("kunkreskintaj",Eco).
kreskanta_ĉe(Eco) --> prep_eco("kreskanta ĉe",Eco).
%dentrandaj(Ecoj) --> "dentrandaj ",ecoj(Ecoj).
kapti(Eco) --> prep_eco("kapti",Eco).
disfalantaj(Eco) --> prep_eco("disfalantaj",Eco).
alta(Eco) --> prep_eco("alta",Eco).


prep_eco(Prep,Eco) -->  { atom(Eco), atom_codes(Eco,E) }, Prep, " ", E.
prep_eco(Prep,(E1;E2)) --> Prep, " ", ecoj((E1;E2)).
prep_eco(Prep,(E1-E2)) --> Prep, " ", ecoj(E1-E2).
prep_eco(Prep,Ecoj) -->  { is_list(Ecoj) }, Prep, " ", ecoj(Ecoj).
prep_eco(Prep,Eco) -->  { compound(Eco), arg_listo(Eco,E) }, Prep, " ", E.
prep_eco(Prep,Ecoj) --> Prep, ecoj([Ecoj]).

'-'(A1,A2) --> { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " ĝis ", C2.

kaj([A1,A2]) --> A1, " kaj", A2. % { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " kaj ", C2.
kaj(A1,A2) --> A1, " kaj ", A2. % { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " kaj ", C2.
aŭ(A1,A2) --> { A2 = (A3;A4), ! }, A1, ", ", aŭ(A3,A4).
aŭ(A1,A2) --> A1, " aŭ ", A2. % { atom_codes(A1,C1), atom_codes(A2,C2) }, C1, " aŭ ", C2.

/*
% konjunkcio
ĉeno((Unu,Du)) :- atomic(Unu), atomic(Du), atomic_list_concat([Unu,Du],' kaj ',Dif), d(Dif), !.
ĉeno((Unu,Pli)) :- ĉeno(Pli,P), atomic_list_concat([Unu,P],', ',Dif), d(Dif).

% disjunkcio
ĉeno((Unu;Du)) :- atomic(Unu), atomic(Du), atomic_list_concat([Unu,Du],' aŭ ',Dif), d(Dif), !.
ĉeno((Unu;Pli)) :- ĉeno(Pli,P), atomic_list_concat([Unu,P],', ',Dif), d(Dif).
*/

