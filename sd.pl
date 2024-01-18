:-use_module(library(clpfd)).
:-use_module(library(clpb)).
transport(Cout,L) :-
    % Liste des quantités envoyée Lij = UiMj de l'usine i au magasin j

    L = [ U1M1, U1M2, U1M3, U1M4,
          U2M1, U2M2, U2M3, U2M4,
          U3M1, U3M2, U3M3, U3M4],

    % Contrainte cout

    Cout #= U1M1*7+ U1M2*8+ U1M3*5+ U1M4*19+U2M1*3+ U2M2*7+ U2M3*2+ U2M4*11+U3M1*9+ U3M2*5+ U3M3*10+ U3M4*18,

   % Les demandes a satisfaire

    U1M1 + U2M1 + U3M1 #= 2,
    U1M2 + U2M2 + U3M2 #= 2,
    U1M3 + U2M3 + U3M3 #= 6,
    U1M4 + U2M4 + U3M4 #= 3,

    % les capacités a ne pas depasser

    [U1M1, U1M2, U1M3, U1M4] ins 0..4,
    [U2M1, U2M2, U2M3, U2M4] ins 0..6,
    [U3M1, U3M2, U3M3, U3M4] ins 0..3,

    % Prendre la solution avec un cout minimal

    once(labeling([min(Cout)], L)).
% ------------------------------------------------------------------------
% Probléme de localisation d'entropots
maintenir(C11,C21,C51,E1):- (C11+C21+C51#=0#/\E1#=0) #\/ (C11+C21+C51#>0#/\E1#=1).
maintenir(C12,C32,E2):- (C12+C32#=0#/\E2#=0) #\/ (C12+C32#>0#/\E2#=1).
maintenir(C23,C33,C43,C53,E3):- (C23+C33+C43+C53#=0#/\E3#=0) #\/ (C23+C33+C43+C53#>0#/\E3#=1).

localisation_entropot(L,Cout):-
    L = [C11,C12,C13,
         C21,C22,C23,
         C31,C32,C33,
         C41,C42,C43,
         C51,C52,C53],

    [E1,E2,E3] ins 0..1,

    L ins 0..1,

    C11+C12#=1,C13#=0,
    C21+C23#=1,C22#=0,
    C32+C33#=1,C31#=0,
    C41+C42#=0,C43#=1,
    C51+C53#=1,C52#=0,

    Cout_transport#=5*C11+7*C12+4*C21+1*C23+2*C32+5*C33+4*C43+3*C51+8*C53,
    Cout_maintenance#= 18*E1+10*E2+28*E3,

    Cout #= Cout_transport + Cout_maintenance,

    maintenir(C11,C21,C51,E1),
    maintenir(C12,C32,E2),
    maintenir(C23,C33,C43,C53,E3),

    Cout#=<60,

    once(labeling([min(Cout)],L)).
% ------------------------------------------------------------------------
% Ordonancement PERT
date_plutot_j(X):-
    X = [A,B,C,D,E,F,G,H,I,J],
    X ins 0..30,
    B #>= A + 5, C #>= A + 5, D #>= A + 5,
    E #>= B + 4,
    F #>= C + 3, G #>= C + 3,
    F #>= D + 2,
    H #>= E + 1,
    I #>= F + 5,
    I #>= G + 4,
    J #>= H + 3,
    J #>= I + 2,
    once(labeling([min(J)],X)),
    write(X).

dates_flexibles(A,B,C,D,E,F,G,H,I,J):-
    [A,B,C,D,E,F,G,H,I,J] ins 0..30,
    J#=15,
    B #>= A + 5, C #>= A + 5, D #>= A + 5,
    E #>= B + 4,
    F #>= C + 3, G #>= C + 3,
    F #>= D + 2,
    H #>= E + 1,
    I #>= F + 5,
    I #>= G + 4,
    J #>= H + 3,
    J #>= I + 2.

% ------------------------------------------------------------------------
% Ordonnancement disjonctif

date_plus_tot_j(X) :-
    X = [A,B,C,D,E,F,G,H,I,J],
    X ins 0..30,
    B #>= A + 5, C #>= A + 5, D #>= A + 5,
    E #>= B + 4,
    F #>= C + 3, G #>= C + 3,
    F #>= D + 2,
    H #>= E + 1,
    I #>= F + 5,
    I #>= G + 4,
    J #>= H + 3,
    J #>= I + 2,
    ((E #< F) #\/ (E #> (F + 4))),
    (((F + 4) #< G) #\/ ((G + 3) #< F)),
    ((E #< G) #\/ (E #> (G + 3))),
    once(labeling([min(J)],X)).

% ------------------------------------------------------------------------
% Problème d’emploi du temps
date_session(L) :-
    L = [A,B,C,D,E,F,G,H,I,J,K],
    L ins 1..4,
    J#>E,K#>D,K#>F,
    A#\=I,J#\=I,I#\=E,E#\=C,C#\=F,F#\=G,D#\=H,B#\=D,K#\=E,
    all_different([B,I,H,G]),
    all_different([A,G,E]),
    all_different([B,H,K]),
    all_different([B,A,H,C]),
    all_different([D,F,J]),
    label(L),
    write(L).

date_sessions(L):-
    L = [ A, B, C, D, E, F, G, H, I, J, K],
    % une demi-journee sera représenté ici par une unité de temps
    L ins 1..4,
    %contraintes meme temps
    A #\= J,
    J #\= I,
    I #\= E,
    E #\= C,
    C #\= F,
    F #\= G,
    D #\= H,
    B #\= D,
    K #\= E,
    all_different([B, I, H, G]),
    all_different([A, G, E]),
    all_different([B, H, K]),
    all_different([A, B, C, H]),
    all_different([D, F, G]),
    %contraintes precedence
    E #< J,
    D #< K,
    F #< K,
    label(L),
    write(L).

salles_sessions(Salle) :-
    % contraintes dates et salles
    [A, B, C, D, E, F, G, H, I, J, K] ins 1..4,
    Salle = [SalleA, SalleB, SalleC, SalleD, SalleE, SalleF, SalleG, SalleH, SalleI, SalleJ, SalleK],
    Salle ins 1..3,
    % contraintes sessions
    all_different([A, B, C, H]),
    all_different([D, F, G]),
    all_different([A, G, E]),
    all_different([B, H, K]),
    all_different([B, I, H, G]),
    C #\= F, E #\= C, I #\= J, I #\= E, J #\= A,
    K #\= E, D #\= H, F #\= G,
    % contraintes precedence
    E #< J, D #< K, F #< K,
    % contraintes d'affectation
    DateSalle = [(A,SalleA), (B,SalleB), (C,SalleC),
                 (D,SalleD), (E,SalleE), (F,SalleF),
                 (G,SalleG), (H,SalleH), (I,SalleI),
                 (J,SalleJ), (K,SalleK)],
    % contrainte de tous différents
    tous_diff_couples(DateSalle),
    % recherche de solutions
    label(Salle),
    label([A, B, C, D, E, F, G, H, I, J, K]).

tous_diff_couples([]).
tous_diff_couples([(X1,X2)|L]) :-
    pas_dans_couples(X1,X2,L),
    tous_diff_couples(L).

pas_dans_couples(_,_,[]).
pas_dans_couples(X1,X2,[(Y1,Y2)|L]) :-
    (X1 #\= Y1 #\/  X2 #\= Y2),
    pas_dans_couples(X1,X2,L).


% ---------------------------------------------------------------------------------------------
% Réunion

dispojean(540,600).
dispojean(810,855).
dispojean(3525,3575).
dispojean(2340, 2460).
dispojean(3360, 3510).
dispojean(5040, 5070).
dispojean(6360, 6445).

dispomarie(600, 630).
dispomarie(750, 795).
dispomarie(2175, 2265).
dispomarie(2340, 2460).
dispomarie(3420, 3510).
dispomarie(4800, 5130).

dispopierre(540, 690).
dispopierre(750, 900).
dispopierre(1980, 2100).
dispopierre(2160, 2520).
dispopierre(3480, 3565).
dispopierre(4800, 4920).
dispopierre(5520, 5610).
dispopierre(6300, 6565).

dispojacque(630, 900).
dispojacque(1920, 2040).
dispojacque(2280, 2400).
dispojacque(3360, 3540).
dispojacque(3720, 3825).
dispojacque(4905, 4995).
dispojacque(6240, 6390).

disposalle(480, 720).
disposalle(2280, 2400).
disposalle(3360, 3420).
disposalle(5040, 5160).
disposalle(6240, 6360).

reunion(Debut,Fin):-
    Fin-Debut #= 45,
    disponible1(Debut,Fin),
    disponible2(Debut,Fin),
    disponible3(Debut,Fin),
    disponible4(Debut,Fin),
    disponible5(Debut,Fin).

disponible1(X,Y):-
    dispojean(X1,Y1),X1#=<X,Y#=<Y1.
disponible2(X,Y):-
    dispomarie(X1,Y1),X1#=<X,Y#=<Y1.
disponible3(X,Y):-
    dispopierre(X1,Y1),X1#=<X,Y#=<Y1.
disponible4(X,Y):-
    dispojacque(X1,Y1),X1#=<X,Y#=<Y1.
disponible5(X,Y):-
    disposalle(X1,Y1),X1#=<X,Y#=<Y1.

% -----------------------------------------------------------------------------------------------------------
% Qui a Gagnéla Médaille d’Or ?
medaille(Personnes):-
	Personnes = [personne(personne1,_,_,_,_,_),
			 personne(personne2,_,_,_,_,_),
			 personne(personne3,_,_,_,_,_),
			 personne(personne4,_,_,_,_,_),
			 personne(personne5,_,_,_,_,_)],

	Contraintes=
	[
	personne(_,		_,	pierre,	brascass,	_,		_),
	personne(_,		cinq,	_,		entorse,	_,		_),
	personne(_,		_,	paul,		_,	equitation,		_),
	personne(Paul,	_,	_,		_,	equitation,		_),
	personne(_,		_,	_,		_,	sautaperche,		cafe),
	personne(_,		_,	_,		insolation,	_,		jus),
	personne(_,		_,	_,		rhume,		decathlon,	_),
	personne(_,		or,	_,		_,		_,		eau),
	personne(The,		_,	andre,		_,		_,		the),
	personne(personne3,	_,	_,		_,		escrime,	_),
	personne(personne1,	_,	_,	cocardeoeil,		_,		_),
	personne(Qua,		quatre,	_,	_,		_,		_),
	personne(Lait,	_,	_,		_,		_,		lait),
	personne(Bron,	bronze,	_,	_,		_,		_),
	personne(Jean,	_,		jean,	_,		_,		_),
	personne(Cocard,	_,	_,	cocardeoeil,		_,		_),
	personne(Rolant,	_,	rolant,	_,		_,		_),
	personne(	_,	_,	_,		_,		box,		_),
	personne(	_,	argent,_,		_,		_,		_)
	],

	droit(Paul,Rolant),
	cote(Lait,Qua),
	cote(The,Bron),
	cote(The,Bron),
	cote(Jean,Cocard),

	ajuste(Contraintes,Personnes).

%definir les contraintes

droit(personne2,personne1).
droit(personne3,personne2).
droit(personne4,personne3).
droit(personne5,personne4).

cote(H1,H2) :- droit(H1,H2);droit(H2,H1).

ajuste([],_).
ajuste([H|T],List) :-
	member(H,List),
	ajuste(T,List).


