:-use_module(library(clpfd)).
:-use_module(library(clpb)).

% Donn�es du probl�me
% Dur�es des t�ches pour chaque produit
tache(produit1, 1, 2,3).
tache(produit1, 2, 4,1).
tache(produit1, 3, 3,2).
tache(produit2, 1, 3,1).
tache(produit2, 2, 2,2).
tache(produit3, 1, 1,3).
tache(produit3, 2, 3,1).

% Predicat principal
% Predicat principal
planification(P1, P2, P3, D) :-
    % Variables : P1, P2 et P3 sont les s�quences des t�ches pour chaque produit, D est la dur�e totale
    % On impose que les s�quences soient des listes de t�ches, et qu'elles contiennent toutes les t�ches n�cessaires
    length(P1, 3), length(P2, 2), length(P3, 2),
    permutation([1, 2, 3], P1),
    permutation([1, 2], P2),
    permutation([1, 2], P3),
    % On impose que chaque t�che ne soit utilis�e qu'une fois et qu'on n'utilise pas la m�me machine en m�me temps
    all_distinct([P1, P2, P3]),
    diff_machine(P1),
    diff_machine(P2),
    diff_machine(P3),
    % On calcule la dur�e totale D en faisant la somme des dur�es des t�ches pour chaque produit
    duree_sequence(P1, D1), duree_sequence(P2, D2), duree_sequence(P3, D3),
    D is D1 + D2 + D3.

% Predicat pour calculer la dur�e d'une s�quence de t�ches
duree_sequence([], 0).
duree_sequence([T|Q], D) :-
    tache(_, T, Duree, _),
    duree_sequence(Q, DReste),
    D is Duree + DReste.

% Predicat pour v�rifier que les t�ches d'une s�quence ne sont pas r�alis�es en m�me temps sur la m�me machine
diff_machine([]).
diff_machine([_]).
diff_machine([T1, T2|Q]) :-
    tache(_, T1, _, Machine1),
    tache(_, T2, _, Machine2),
    (Machine1 \= Machine2 ; T1 = T2),
    diff_machine([T2|Q]).

