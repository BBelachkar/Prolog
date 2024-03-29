% Mars 2021 : BD pour le BE3 de Ar-S8
:-use_module(library(clpfd)).
servir('Les Guinguettes', 'Heinekin').          servir('Les Guinguettes', 'Leffe').
servir('Les Guinguettes', 'Export 33').         servir('Les Guinguettes', 'Leffe').
servir('Les Guinguettes', 'Pilsner').           servir('Les Guinguettes', 'Kronenbourg').
servir('Les Guinguettes', 'Quintor').

servir('Lucifer Inn', 'Grimbergen').            servir('Lucifer Inn', 'Castagnor').
servir('Lucifer Inn', 'Quintor').               servir('Lucifer Inn', 'Carlsberg').
servir('Lucifer Inn', 'Kanterbrau').            servir('Lucifer Inn', 'Pils').

servir('Cervisia Caupona', 'Force 4').          % 'Cervisia Caupona' = Taverne de Bière
servir('Cervisia Caupona', '1664').             servir('Cervisia Caupona', 'Chtis').
servir('Cervisia Caupona', 'Kronenbourg').      servir('Cervisia Caupona', 'Guinness').

servir('Auberge du Village', 'Fischer').        servir('Auberge du Village', 'Castagnor').
servir('Auberge du Village', 'Desperado').      servir('Auberge du Village', 'Grimbergen').

servir('Opus Satanae', 'Desperado').            servir('Opus Satanae', 'Castagnor').
%-----------------------------------------------------------------------------------

frequenter('Jean', 'Les Guinguettes').          frequenter('Jean', 'Opus Satanae').
frequenter('Jean', 'Lucifer Inn').              frequenter('Jean', 'Cervisia Caupona').
frequenter('Jean', 'Auberge du Village').

frequenter('Marie', 'Lucifer Inn').             frequenter('Marie', 'Cervisia Caupona').

frequenter('Pierre', 'Auberge du Village').     frequenter('Pierre', 'Opus Satanae').

frequenter('Sara', 'Auberge du Village').       frequenter('Sara', 'Lucifer Inn').
frequenter('Sara', 'Cervisia Caupona').
%-----------------------------------------------------------------------------------
aimer('Jean', 'Heinekin').                      aimer('Jean', 'Grimbergen').
aimer('Jean', 'Castagnor').                     aimer('Jean', 'Desperado').

aimer('Marie', 'Force 4').          aimer('Marie', 'Fischer').          aimer('Marie', 'Quintor').

aimer('Pierre', 'Quintor').         aimer('Pierre', '1664').            aimer('Pierre', 'Pilsner').

aimer('Sara', 'Kronenbourg').       aimer('Sara', 'Leffe').             aimer('Sara', 'Guinness').

mon_setof(Terme,Buts,Set):-
    findall(Terme, Buts, Liste),
    setof(Terme, Terme ^ member(Terme, Liste), Set),
    !.
% si Liste=[] dans findall, member �choue et mon_setof �galement. On ajoute donc le "!" et :
mon_setof(_Terme, _Buts, []).
verifier(_,[]).
verifer(Buv,[Bar|Reste]) :-
    frequenter(Buv,Bar),
    verifer(Buv,Reste).
frequenter1(Buv):-
    mon_setof(Bar ,servir(Bar,_), Bars),
    verifier(Buv,Bars).

solve(Y, Z) :-
    Y in 0..9,
    Z in 1..3,
    2*Y - 5*Z #= 3,
    label([Y,Z]).


produit('VHN', v�lo, homme, noir).
produit('BFB', ballon, foot, blanc).
vente('VHN', dupont, 200, paris).
vente('VHN', durand, 150, lyon).
usine(paris_tech, paris, 'VHN').
