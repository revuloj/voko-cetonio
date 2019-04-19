/* -*- Mode: Prolog -*- */

:- encoding(utf8).

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
   "fruktonodo?"...

*/


plgrp(testo1,[
   rizomo(dika,amelriĉa) %,
   ]).

plgrp(testo2,[
   rizomo(dika;amelriĉa) %,
	]).

plgrp(testo3,[
   rizomo(dika) %,
      ]).

alinomo(gimnosperm,nudsemplant).
alinomo(angiosperm,floroplant).

/*
grupoj(semplantoj, [ gimnosperm, angiosperm ]).
grupoj(gimnosperm, [ efedr, velvitŝi, gent, konifer ]).
% floroplantoj:
grupoj(angiosperm, [ anit, magnoli, monokotiledon, roz, aster ]).

grupoj(roz, [ malv, fab ]).
grupoj(aster, [ lami, kampanul ]).

ordoj(angiosperm,
      [amborel, nimfe, aŭstrobajle, klorant, canle, pipr, laŭr, magnoli,...]).


ordoj(magnoli, [ kanel, laŭr, pipr, magnoli ]).
ordoj(komeli, [ arek, po, komelik, zingribr ]).
%ordoj(eŭdikotiledon, [ ceratofil, ranunkol, sabi, prote, troĥodendr, buks, '...' ]).
ordoj(fab, [ zigofil, celastr, oksalid, malfigi, fab, roz, kukurb, fag ]).
ordoj(malv, [  gerani, mirt, krosomat, pikrmani, sapind, huerte, malv, brasik ]).
ordoj(lami, [ ikacin, metenius, gari, gentian, solan, lami, borag ]).
ordoj(kampanul, [ akvifoli, aster, eskaloni, bruni, api, parakrifi, dipsak ]).
*/

familioj(amborel, [ amborel ]).
familioj(nimfe, [ kabomb, hidatel, nimfe ]).
familioj(aŭstrobajle, [aŭstrobajle, ŝisandra, trimenia ]).
familioj(klorant, [ klorant ]).
familioj(kanel, [ kanel, vinter ]).
familioj(pipr, [ aristoloki, pipr, hidnor, saŭrur ]).
familioj(laŭr, [ kalikan, hernandi, monimi, gomorteg, laŭr, siparun ]).
familioj(magnoli, [ anon, eŭpomati, magnoli, degeneri, himantandr, miristiak ]).
familioj(akor, [ akor ]).
familioj(alismat, [ alismat, junkagi, rupi, aponogeton, butom, posidoni, ŝeŭĥceri,
		    arak, hidroĥari, potamogeton, zoster ]).
familioj(petrosavi, [ petrosavi ]).
familioj(dioskore, [ burmani, dioskore, narteki, tak ]).
familioj(pandan, [ kikiant, pandan, velozi ]).
familioj(lil, [ alstremeri, korsi, melanti, filesi, kolĥik, lili, petermani, smilak ]).
familioj(asparag, [ amarilid, irid, asparag, hipoksid, lanari, orĥid, tekofilae, ksantoroe ]).
familioj(arek, [ arek ]).
familioj(po, [ bromeli, erokaŭl, po, restion, ksirid, ciper, junk, rapate, tif ]).
familioj(komelin, [ komelin, hemodor, pontederi ]).
familioj(zingribr, [ kan, helikoni, marant, strelici, kost, lovi, mus, zingibr ]).
familioj(ceratofil, [ ceratofil ]).
familioj(ranunkol, [ berberid, eŭptele, menisperm, cirkaeaster, lardizabal, papav ]).
familioj(sabi, [ sabi ]).
familioj(prote, [ nelumbon, platan, prote ]).
familioj(troĥodendr, [ troĥodendr ]).
familioj(buks, [ buks, didimel ]).
familioj(guner, [ guner, mirotamn ]).
familioj(dileni, [ dileni ]).
familioj(saksifrag, [ altingi, dafnifil, hamamelid, cerkidifil, grosuj, penoni,
		      krasul, halorag, saksifrag ]).
familioj(vit, [ vit ]).
familioj(zigofil, [ krameri, zigofil ]).
familioj(celastr, [ celastr, lepidobotri ]).
familioj(oksalid, [ bruneli, konar, eleokarp, oksalid, cefalot, kunoni, huak ]).
familioj(malfigi, [ aĥari, eŭforbi, raflesi, oĥn, podostem, krisobalan, hiperik, pasiflor, rizofor, klusi, lin, filantant, slok, eritoksil, malfigi, pikrodendr, viol ]).
familioj(fab, [ fab, poligal, kvilaj, surian ]).
familioj(roz, [ barbej, eleagn, roz, kanab, morus, ulm, diraĥm, ramn, urtik ]).
familioj(kukurb, [ anisofil, koriari, kukurb, begoni, korinokarp, datisk, tetramel ]).
familioj(fag, [ betul, fag, mirik, roiptele, kasurain, juglanduj, notofag, tikodendr ]).
familioj(gerani, [ franko, gerani, ledokarp, meliant ]).
familioj(mirt, [ kombret, mirt, pene, litrum, melastomat, enoter % onagr,
							      , voĥisi ]).
familioj(krosomat, [ krosomat, staĥiur, gejsolomat, stafile, strasburgeri ]).
familioj(pikramni, [ pikramni ]).
familioj(sapind, [ anakardi, meli, rut, burser, nitrari, sapind, simarub ]).
familioj(huert, [ dipentodont, gerardin, petene, tapiski ]).
familioj(malv, [ biks, malv, cist, citin, muntingi, sarkolen, dipterokark, neŭrad, sferosepal, timele ]).
familioj(brasik, [ bat, karik, limnant, salbador, brasik, kleom, moring, tovari, kapar, keberlini, resed, tropeol ]).
familioj(santal, [ balanofor, misodendr, opili, ŝepfi, lorant, olak, santal, visk ]).
familioj(berberidopsid, [ ekstoksik, berberidopsid ]).
familioj(kariofil, [ aizo, kariofil, molugin, poligon, amarant, didiere, nepent, portulak, droser, niktagin, simondsi, basel, drosofil, fitolak, talin, kakt, frankeni, plumbagin, tamarik ]).
familioj(kornus, [ kornus, grubi, loas, curtisi, hidrange, nis ]).
familioj(erik, [ aktinidi, erik, polemoni, saraceni, blzamin, fokieri, primul, stirak, cletr, lecitid, roridul, te, eben, mirsin, spot, teofrast ]).
familioj(ikakin, [ onkotek, ikakin ]).
familioj(metenius, [ metenius ]).
familioj(gari, [ eŭkomi, gari ]).
familioj(gentian, [ apokin, logani, gentian, gelsemi, rubi, valia ]).
familioj(solan, [ konvolvul, solan, hidrole, montini, sfenokle ]).
familioj(lami, [ akant, lami, orobanĉ, plantagin, bignoni, lenibulari, paŭlovini, akrofulari, biblid, martini, pedali, stilb, gesneri, hidrostaĥi, ole, frim, verben ]).
familioj(borag, [ borag, kodon, kolden, kordi, ereti, heliotropi, hidrofil, nam, velstedi ]).
familioj(akvifoli, [ akvifoli, kardiopterid, stemonur ]).
familioj(aster, [ aster, kalicer, kampanul, goodeni, meniant, pentafragmat, rose, stilidi ]).
familioj(eskaloni, [ eskaloni ]).
familioj(bruni, [ bruni, kolumeli ]).
familioj(api, [ api, friselini, penanti, arali, miodokarp, pitospor ]).
familioj(parakrifi, [ parakrifi ]).
familioj(dipsak, [ adoks, diervil, lin, kaprifoli, dipsak, morin, valerian ]).

subfamilioj(papav, [ fumari, papav ]).
	 

/*
familio(aceracoj,

akvifoliacoj
amarilidacoj
anakardiacoj
anoniacoj
apiacoj
apocinacoj
araliacoj
araŭkariacoj
arekacoj
*/

familio(aristoloki,
      [
       la('Aristolochiaceae'),
       de('Osterluzeigewächse')
      ],[
	  (staŭdoj,lignoplantoj(sinuantaj))
      ],[
	  folioj(alternaj,glatrandaj),
	  floroj(duseksaj,(stelaj;dulipaj)),
	  tepaloj(plataj;kunkreskintaj;tubecaj;kloŝaj),  
	  stamenoj(6-12,'kolone kunkreskintaj kun la pistilo'),
	  ovolujo(malsupra,kun(fakoj(4-6))),
	  stigmo(sesradia)
      ]).
  
/*
asteracoj
balzaminacoj
bananujacoj
begoniacoj
*/

familio(betul,
	[ la('Betulaceae'), de('Birkengewächse') ],
	[ (arboj,arbedoj) ],
	[
	    amentoj(longformaj),
	    viraj_floroj(po(pluraj),en(akseloj(brakteaj))),
	    tepaloj(apenaŭ(videblaj)),
	    stamenoj(sen(harfaskoj(ĉe(pinto)))),
	    nuksetoj(alaj,sen(ujo))
	]).

    /*
bombakacoj
boragacoj
brasikacoj
bromeliacoj
buksacoj
celastracoj
ciperacoj
cipresacoj
cistacoj
dipsakacoj
*/

familio(eleagn,
	[ la('Eleagnaceae'), de('Ölweidengewächse') ],
	[ (arboj; arbedoj) ],
	[
	    ofte(dornaj),
	    folioj((kontraŭaj;kruce(alternaj)),haraj),
	    floroj(malgrandaj,(unuseksaj;duseksaj),kalikaj),
	    tepaloj(2-4),
	    nukso(ĉirkaŭita_de(kaliko(karna)),simila_al(drupo))
	]).

familio(enoter, % nova nomo: onagr
	[ la('Onagraceae'), de('Nachtkerzengewächse') ],
	[ (herboj(unujaraj);staŭdoj) ],
	[
	    folioj(kontraŭaj;malofte(rozetaj;spiralaj)),
	    floroj(duseksaj,(stelaj;dulipaj)),
	    tepaloj(du;kvar),
	    ovolujo(malsupra),
	    florfundo((tubforma;kalikforma),ofte(okulfrape(kolorigita))),
	    stamenoj(du;kvar;ok),
	    (kapsuloj;nuksoj)
	]).


/*
eŭforbiacoj
*/

familio(fab,
	[ la('Fabaceae'), de('Schmetterlingsblütler') ],
	[ (lignoplantoj;duonarbustoj;staŭdoj;herboj(unujaraj)) ],
	[
	    folioj((alternaj;kruce(alternaj)),kun(kromfolioj)),
	    floroj(dulipaj,papiliformaj),
	    sepaloj(5,ofte(kunkreskintaj)),
	    petaloj(5,'estantaj la supra la flago, la du flankaj la aloj kaj la du malsupraj la ŝipeto'),
	    stamenoj(10,kunkreskintaj('aŭ ĉiuj je fermita aŭ nur naŭ je supre malfermita tubo ĉirkaŭanta  la ovolujon')),
	    pistilo(1),
	    guŝoj(klape(malfermiĝantaj),(disfalantaj('en partojn unusemajn');helice(turniĝantaj)))
	]).


familio(fag,
	[ la('Fagaceae'), de('Buchengewächse') ],
	[ lignoplantoj(monoikaj) ],
	[
	    folioj((spiralaj;alternaj)),
	    kromfolioj(frue(defalantaj)),
	    tepaloj(apenaŭ(videblaj)),
	    ovolujo(supra,kun(fakoj(3-6))),
	    nukseto((unuopa;pluropa),ĉirkaŭita_de(fruktujo))
	    ]).

/*
filadelfacoj -> hortensiacoj -> hidrangeacoj
flakurtiacoj
*/

% nun subfamilio de papaveracoj
subfamilio(fumari, 
      [
       la('Fumarioideae'),
       de('Erdrauchgewächse')
      ],
      [
       (herboj(unujaraj),staŭdoj),
       geofitoj
      ],
      [
       folioj(alternaj,dividitaj,kun(suko(akva))),
       grapoloj,
       floro((duflanka;dulipa),ofte(sprona)),
       stamenoj(ses,ofte('po tri en du faskoj')),
       (silikvo(senvanda);nukseto)
      ]).
    
  
/*  

gencianacoj
geraniacoj
globulariacoj
gnetacoj
granatujacoj
*/

familio(grosuj,
	[ la('Grossulariaceae'), de('Stachelbeergewächse') ],
	[ arbedoj(someraj) ],
	[
	    folioj(kruce(alternaj),lobaj,sen(kromfolioj)),
	    tigoj(foje(kun(elstaraĵoj(pikaj,(simplaj;dividitaj))))),
	    grapoloj(malmulfloraj-multfloraj,(starantaj;pendantaj)),
	    floroj(stelaj),
	    petaloj(5),
	    sepaloj(liberaj),
	    ovolujo(malsupra),
	    beroj(foje(pikaj;haraj)),
	    genro(sola(ribo))
	]).

familio(halorag,
	[ la('Haloragaceae'), de('Tausendblattgewächse') ],
	[ ofte(akvoplantoj), ofte(monoikaj) ],
	[
	    floroj(malgrandaj,nefrapaj,ofte(unuseksaj)),
	    tepaloj(kvar)
	]).
	

familio(hidrange,
	[ la('Hydrangeaceae'), de('Hortensiengewächse') ],
	[ arbedoj, lianoj, duonarbustoj, malofte(staŭdoj) ],
	[
	    folioj(kontraŭaj,sen(kromfolioj)),
	    (cumoj;paniklo;korimbaro),
	    floroj(duseksaj,malofte(unuseksaj),stelaj,(multaj,malgrandaj);(malmultaj,grandaj)),
	    sepaloj(4-12),
	    petaloj(4-12,kunkreskintaj('ĉe-baze';komplete)),
	    stamenoj(4-200,en(cirkloj)),
	    pistiloj(2-12),
	    stilusoj(ofte(disbranĉiĝantaj)),
	    ovolujoj(malsupraj,kun(fakoj(1-12)))
	]).

/*
hiperikacoj
hipokaŝtanacoj
iridacoj
*/

familio(juglanduj,
	[ la('Juglandaceae'), de('Walnussgewächse') ],
	[ arboj ],
	[
	    folioj(nepare(pinataj),arome(odorantaj)),
	    amentoj(pendantaj,ĉe(ligno(antaŭjara))),
	    virinaj_floroj(2-3,fine_de(ŝosoj(tiuĉijaraj))),
	    ovolujo(malsupra),
	    drupoj(kun(kovro(fibra,verdeta,diŝiriĝanta)))
	]).

    /*
junkacoj
kampanulacoj
*/

familio(kanab,
	[ la('Cannbaceae'), de('Hanfgewächse') ],
	[ (staŭdoj, herboj(unujaraj)), dioikaj ],
	[
	    sen(laktosuko),
	    folioj(fingraj;lobaj),
	    floroj(unuseksaj,foje(strobiloformaj)),
	    nuksetoj
	]).

/*
kaprifoliacoj
kariofilacoj
kenopodiacoj
konvolvulacoj
kornusacoj
*/

familio(krasul,
	[ la('Crassulaceae'), de('Dickblattgewächse') ],
	[ (staŭdoj, herboj(unujaraj)) ],
	[
	    folioj((kontraŭaj;kruce(alternaj);bazaj),sukaj,simplaj,plataj-cilindraj,sen(kromfolioj)),
	    cumoj,
	    floroj(stelaj,duseksaj),
	    petaloj(4-5,malofte(tri),(liberaj;kunkreskintaj)),
	    stamenoj(multaj;duoble(petaloj)),
	    ovolujoj(supraj,ofte(pluropaj),(liberaj;kunkreskintaj('ĉe-baze'))),
	    folikloj
	]).
	    

    /*
kukurbacoj
lamiacoj
laŭracoj
likopodiacoj
liliacoj
linacoj
*/

familio(litrum,
	[ la('Lythraceae'), de('Blutweiderichgewächse') ],
	[ (herboj(unujaraj);staŭdoj) ],
	[
	    folioj((ofte(kontraŭaj);malofte(rozete;spirale)),glatrandaj),
	    floroj(stelaj),
	    virinaj_floroj(kun(fundo(kalikforma;tubforma))),
	    stamenoj(duoble(petaloj)),
	    ovolujoj(ofte(du)),
	    kapsuloj
	]).

    /*
lorantacoj
magnoliacoj
malvacoj
meliacoj
mimozacoj
miristikacoj
mirtacoj
*/

familio(morus,
	[ la('Moraceae'), de('Maulbeergewächse') ],
	[ (arboj,arbedoj) ],
	[
	    suko(lakta),
	    folioj(nedividitaj;lobaj),
	    tirsoj,
	    floroj(unuseksaj),
	    grupfruktoj(karnaj)
	]).

    /*
nimfeacoj
oksalidacoj
oleacoj
*/


/*
orkidacoj
*/
    
familio(papav,
      [
       la('Papaveraceae'),
       de('Mohngewächse')
      ],
      [
       (herboj(unujaraj),staŭdoj)
      ],
      [
       suko(ofte(lakta)),
       floroj(stelaj),
       sepaloj(du,frue(defalantaj)),
       petaloj(kvar),
       stamenoj(multaj),
       ovolujo(supra,(dufolia;multfolia)),
       kapsulo(kun(poroj;klapoj))
      ]).  
  
/*
pedaliacoj
peoniacoj
pinacoj
piprujacoj
plantagacoj
*/

familio(platan,
	[ la('Platanaceae'), de('Platanengewächse') ],
	[ arboj(monoikaj) ],
	[
	    trunkoŝelo(defalanta(en(pecoj)),pro_tio(makulita('verde-flave-brune'))),
	    folioj(kun(loboj(3-7)),neegale(dentrandaj)),
	    kromfolioj(grandaj,foliformaj,tigoĉirkaŭaj),
	    floroj(unuseksaj),
	    floraroj(pendantaj,globaj,'ĉe-tigaj'),
	    stamenoj(3-8),
	    ovolujoj(3-8,liberaj),
	    genro(sola(platano))
	]).
	
		   

/*
poacoj
poligonacoj
primolacoj
ramnacoj
ranunkolacoj
rezedacoj

*/

familio(roz,
	[ la('Rosaceae'),de('Rosengewächse') ],
	[ (herboj(unujaraj); staŭdoj; lignoplantoj) ],
	[
	    folioj(kruce(alternaj),ofte(kun(kromfolioj))),
	    floroj(stelaj,duseksaj,ofte(kun(sepaloj,petaloj))),
	    stamenoj(kvin;'du- ĝis kvaroble tiom, kiom petaloj'),
	    ovolujoj(multaj-unu,(liberaj;false(kunkreskintaj))),
	    (kapsuloj;nuksoj;drupoj;plurfruktoj)
	]).

/*	    
rubiacoj
rusulacoj
*/

familio(rut,
	[ la('Rutaceae'), de('Rautengewächse') ],
	[ (arboj;duonarbustoj;staŭdoj) ],
	[
	    folioj(travideble(punktitaj(pro(oleglandoj)))),
	    forte(odorantoj),
	    floroj((stelaj;dulipaj)),
	    tepaloj(4-5),
	    florakso(etendita((inter(stamenoj);super(stamenoj)),je(disko))),
	    kapsuloj
	]).
	    
	    


familio(saksifrag,
	[ la('Saxifragaceae'), de('Steinbrechgewächse') ],
	[ (herboj(unujaraj);staŭdoj) ],
	[
	    folioj((kruce(alternaj);malofte(kontraŭaj)),sen(kromfolioj)),
	    floroj(duseksaj,stelaj),
	    petaloj(ofte(kvin)),
	    ovolujoj(du,supraj,kunkreskintaj('ĉe-baze')),
	    kapsuloj
	]).


/*	    
salikacoj
sapotacoj
sapotacoj
*/

familio(saraceni,
	[ la('Sarraceniaceae'), de('Schlauchpflanzengewächse') ],
	[ staŭdoj(karnovoraj) ],
	[
	    rizomo,
	    folioj(ofte(bazaj,rozete(aranĝitaj)),foje(kruce(alternaj),ĉe(tigo(rekta)),tubformaj,uzataj(por(kapti(insektojn)))),sen(kromfolioj)),
	    floroj(stelaj,ofte(unuopaj)),
	    sepaloj(3-6),
	    stamenoj(10-20),
	    ovolujo(supra,kun(fakoj('3 aŭ 5'))),
	    kapsuloj(kun(semoj(multaj,malgrandaj,alaj)))
	]).


familio(simarub,
	[ la('Simaroubaceae'), de('Bittereschengewächse') ],
	[ (arboj; arbedoj) ],
	[
	    folioj(ofte(pinataj)),
	    florumo(duobla),
	    tepaloj(po(3-7)),
	    fruktoj(multformaj)
	]).

    /*
skrofulariacoj
solanacoj
sterkuliacoj
taksusacoj
tamarikacoj
teacoj
tiliacoj
timeleacoj
*/

familio(ulm,
	[ la('Ulmaceae'), de('Ulmengewächse') ],
	[ lignoplantoj ],
	[
	    folioj(alternaj, nesimetriaj),
	    kromfolioj(frue(defalantaj)),
	    floroj(unuopaj;en(faskoj)),
	    tepaloj(4-5),
	    stamenoj(4-5),
	    pistiloj(2),
	    (nuksoj(alaj);drupoj)
	]).

    
familio(urtik,
	[ la('Urticaceae'), de('Brennnesselgewächse') ],
	[ (herboj,staŭdoj) ],
	[
	    ofte(kun(brulharoj)),
	    folioj((kontraŭaj;kruce(alternaj)),segildentaj),
	    (umbeloj(kunmetitaj);cumoj(sidantaj);tirsoj),
	    tepaloj(ofte(4-5),malofte(2-6)),
	    ovolujo(supra,unufaka)
	]).

    /*
valerianacoj
verbenacoj
violacoj
vitacoj
zigofilacoj
zingibracoj

	*/
