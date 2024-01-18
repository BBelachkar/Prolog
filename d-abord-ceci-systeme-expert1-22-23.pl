% kate: hl Prolog;
% Système de règles de production

:- dynamic [but/1, fait/1].
:- op(954,xfy,et).
 
go :- chainage_avant.        
go :- write('fini'), nl.
chainage_avant :-     
    regle(ID),
    write('règle activée : '), write(ID),write('.'), nl, !,         
    chainage_avant.

/* prédicats de gestion de la base */
ajouter(X) :- \+call(X) , assert(X), !.        % on peut utiliser "clause"
ajouter(_).

supprimer(X) :- call(X) , retract(X), !.        % on enlève X que s'il est là
supprimer(_).

/* réussit toujours et ne fait rien */
alors.                    
si.        % éventuellement (si on décidait de garder "si")

% nettoyage et rechargement
charger(X) :- 
    abolish(fait/1),
    abolish(but/1), 
    abolish(regle/1), 
    consult(X).
    

executer(A et B) :- !, executer(A),executer(B).
executer(A) :- call(A).    

%===========================================================
% TEST 1
%but(allumer_lampe).
%fait(lampe_eteinte).

% TEST 2 : mettre les 2 lignes au dessus en commentaire et décommenter ces 2 lignes :
but(eteindre_lampe).
fait(lampe_allumee).

regle(1)     :-   
    si, 
        but(allumer_lampe),
        fait(lampe_allumee),        %noter la "virgule" ici
    alors,    
        supprimer(but(allumer_lampe)).    % et là

regle(2)     :-    
    si,
        but(eteindre_lampe),
        fait(lampe_eteinte),             
    alors,     
        supprimer(but(eteindre_lampe)).

regle(3)      :-    
    si,
        but(allumer_lampe),        % règle-2' précédente
        fait(lampe_eteinte) ,
        \+but(tourner_interrupteur),        % on ne marque pas encore
    alors,   
        ajouter(but(tourner_interrupteur)).    % les règles

regle(4)     :-    
    si,
        but(eteindre_lampe),            % règle nouvelle
        fait(lampe_allumee) ,            % pour prévoir toute situation 
        \+but(tourner_interrupteur),         
    alors,    
        ajouter(but(tourner_interrupteur)).    
        
regle(5)     :-    
    si,
        but(tourner_interrupteur),
        fait(lampe_allumee),
    alors,    
        supprimer(but(tourner_interrupteur)),
        supprimer(fait(lampe_allumee)),
        ajouter(fait(lampe_eteinte)).

regle(6)     :-    
    si,
        but(tourner_interrupteur),
        fait(lampe_eteinte),
    alors,    
        supprimer(but(tourner_interrupteur)) ,
        supprimer(fait(lampe_eteinte)),
        ajouter(fait(lampe_allumee)).
        
        
