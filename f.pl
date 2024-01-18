:-use_module(library(clpfd)).
:-use_module(library(clpb)).

% Données du problème
% Durées des tâches pour chaque produit
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
    % Variables : P1, P2 et P3 sont les séquences des tâches pour chaque produit, D est la durée totale
    % On impose que les séquences soient des listes de tâches, et qu'elles contiennent toutes les tâches nécessaires
    length(P1, 3), length(P2, 2), length(P3, 2),
    permutation([1, 2, 3], P1),
    permutation([1, 2], P2),
    permutation([1, 2], P3),
    % On impose que chaque tâche ne soit utilisée qu'une fois et qu'on n'utilise pas la même machine en même temps
    all_distinct([P1, P2, P3]),
    diff_machine(P1),
    diff_machine(P2),
    diff_machine(P3),
    % On calcule la durée totale D en faisant la somme des durées des tâches pour chaque produit
    duree_sequence(P1, D1), duree_sequence(P2, D2), duree_sequence(P3, D3),
    D is D1 + D2 + D3.

% Predicat pour calculer la durée d'une séquence de tâches
duree_sequence([], 0).
duree_sequence([T|Q], D) :-
    tache(_, T, Duree, _),
    duree_sequence(Q, DReste),
    D is Duree + DReste.

% Predicat pour vérifier que les tâches d'une séquence ne sont pas réalisées en même temps sur la même machine
diff_machine([]).
diff_machine([_]).
diff_machine([T1, T2|Q]) :-
    tache(_, T1, _, Machine1),
    tache(_, T2, _, Machine2),
    (Machine1 \= Machine2 ; T1 = T2),
    diff_machine([T2|Q]).

