% Mars 2023
% Ex pb Transport avec SWI Prolog

:- use_module(library(clpfd)).

cout(u1, m1, 7).			cout(u1, m2, 8).			cout(u1, m3, 5).		cout(u1, m4, 19).
cout(u2, m1, 3).			cout(u2, m2, 7).			cout(u2, m3, 2).		cout(u2, m4, 11).
cout(u3, m1, 9).			cout(u3, m2, 5).			cout(u3, m3, 10).		cout(u3, m4, 18).

demande(m1, 2).		demande(m2, 2).		demande(m3, 6).		demande(m4, 3).
capacite(u1, 4).		capacite(u2, 6).			capacite(u3, 3).


go(Liste_u1_u2_u3, Somme_couts) :-
	Liste_u1=[ U1M1, U1M2, U1M3, U1M4], Liste_u1 ins 0..4,
	Liste_u2=[ U2M1, U2M2, U2M3, U2M4], Liste_u2 ins 0..6,
	Liste_u3=[ U3M1, U3M2, U3M3, U3M4], Liste_u3 ins 0..3,

	Liste_u1_u2_u3=
		[ U1M1, U1M2, U1M3, U1M4,
		  U2M1, U2M2, U2M3, U2M4,
		  U3M1, U3M2, U3M3, U3M4
		 ],
	
	% extraire les couts pour u1 contraint à 4 unités
	capacite(u1, Capa_u1), sum(Liste_u1, #=, Capa_u1),
	findall(C, (between(1,4,X), atom_concat(m,X,Var),cout(u1, Var, C)), L_couts_u1),
	scalar_product(L_couts_u1, Liste_u1, #=,Somme_couts_u1),
	
	% extraire les couts pour u2 (6 unités)
	capacite(u2, Capa_u2), sum(Liste_u2, #=, Capa_u2),
	findall(C, (between(1,4,X), atom_concat(m,X,Var),cout(u2, Var, C)), L_couts_u2),
	scalar_product(L_couts_u2, Liste_u2, #=,Somme_couts_u2),
	
	% extraire les couts pour u3 (limité à 3 unités)
	capacite(u3, Capa_u3), sum(Liste_u3, #=, Capa_u3),
	findall(C, (between(1,4,X), atom_concat(m,X,Var),cout(u3, Var, C)), L_couts_u3),
	scalar_product(L_couts_u3, Liste_u3, #=,Somme_couts_u3),
	
	sum([U1M1, U2M1, U3M1], #=, 2),
	sum([U1M2, U2M2, U3M2], #=, 2),
	sum([U1M3, U2M2, U3M3], #=, 6),
	sum([U1M4, U2M4, U3M4], #=, 3),
	
	Somme_couts #= Somme_couts_u1 + Somme_couts_u2 + Somme_couts_u3,
	labeling([min(Somme_couts)], Liste_u1_u2_u3)
	.
	
/*
go(L, C), print(L).
[0,0,4,0,
 1,1,1,3,
 1,1,1,0]
L = [0, 0, 4, 0, 1, 1, 1, 3, 1|...],
C = 89 ;

*/