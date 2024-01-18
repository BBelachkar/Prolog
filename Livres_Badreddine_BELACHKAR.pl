% BD. des livres
:-use_module(library(aggregate)).
:-use_module(library(clpfd)).


:- use_module(library(lists)).

livres(auteur('Victor', 'Hugo'), [('Juliette Drouet',  32), ('Notre Dame de Paris', 45), ('Les Misérables', 35), ('Quatre Vingt Treize', 24), ('Feuilles d automne', 30), ('Les Contemplations', 25)]).
livres(auteur('Léo', 'Ferré'), [('Testament Phonographe', 25), ('La méthode', 25), ('Benoit Misère', 30)]).
livres(auteur('Max', 'Weber'), [('Economie et Société', 24), ('Le savant et le Politique', 29), ('Théorie de la science', 34), ('La bourse', 25)]).
livres(auteur('Blaise', 'Pascal'), [('Pensées', 25), ('De l esprit Géométrique', 45)]).
livres(auteur('Confucius', 'Confucius'), [('Confucius', 35), ('La morale', 30), ('Les entretiens', 25)]).
livres(auteur('Jacques', 'Lacan'), [('D un autre à l autre', 30), ('Mon enseignement', 50)]).
livres(auteur('Sigmund', 'Freud'), [('Sur le rêve', 30), ('Totem et Tabou', 25), ('MétaPsycologie', 40)]).
livres(auteur('Michel', 'Foucault'), [('Surveiller et punir', 34), ('Histoire de la folie', 25), ('L ordre du discours', 35)]).
livres(auteur('Jacques', 'Derrida'), [('Feu la cendre', 30), ('Mémoire d aveugle' , 20), ('Voiles', 25 ) , ('Demeure' , 35), ('Position', 20)]).
livres(auteur('Michel', 'Serres'), [('Atlas, Philosophie des réseaux', 30), ('Tiers Instruit', 25)]).
livres(auteur('Simone', 'Weil'), [('Pensées', 30), ('Attente de Dieu', 20), ('La Condition Ouvrière', 35) , ('Cahiers', 25)]).
livres(auteur('Simone', 'De Beauvoir'), [('La Force des Choses', 35), ('L Invitée', 22), ('Les Mandarins', 35) , ('Tout Compte Fait', 20)]).
livres(auteur('Honoré', 'de BALZAC'), [('La bourse', 35), ('Adieu', 32),('Episode sous la terreur', 30)]).
livres(auteur('Enki', 'Bilal'), [('Le Monstre - RDV à Paris', 15), ('Quatre', 13), ('Hors jeu', 14)]).
livres(auteur('Paul', 'Auster'), [('Moon Palace', 20), ('Le Livre des illusions', 25)]).
livres(auteur('Marcel', 'Proust'), [('Du côté de chez Swann', 30), ('A la recherche du temps perdu', 50)]).
livres(auteur('Jean', 'Cocteau'), [('Les enfants terribles', 25), ('La machine infernale', 30)]).
livres(auteur('Jean', 'Paul Sartre'), [('La nausée', 20), ('Huis clos', 25)]).
livres(auteur('Paul', 'Valéry'), [('Charmes', 25), ('Cahiers', 35)]).
livres(auteur('Oussama', 'Weil'), [('Exemple', 45), ('Attente de Dieu', 20), ('La Condition Ouvrière', 35)]).
livres(auteur('Ahmed', 'Weil'), [('Exemple', 45), ('Attente de Dieu', 20), ('La Condition Ouvrière', 35)]).
livres(auteur('ka', 'Weil'), [('Exele', 45), ('AttenteDieu', 20), ('La Condition Ouvrière', 35)]).
% Pour les tests
% Ecrire les prédicats répondant aux requêtes suivantes :

% auteurs_identiques_nom/1 : cherche les auteurs ayant le même nom de famille
auteurs_identiques_nom(L) :-
    livres(auteur(A, Nom), L1),
    livres(auteur(B, AutreNom), L2),
    dif(livres(auteur(A,Nom), L1), livres(auteur(B,AutreNom), L2)),
    Nom = AutreNom,
    A @=< B,
    append([auteur(A, Nom)],[auteur(B, AutreNom)],R),
    list_to_set(R,L).


% write(A),write(' '),write(Nom),write(' et '),write(B),write(' '),write(AutreNom),Nom \= ''.

% auteurs_identiques_prenom/1 : cherche les auteurs ayant le même prénom
auteurs_identiques_prenom() :-
    livres(auteur(Prenom, A), L1),
    livres(auteur(AutrePrenom, B),L2),
    livres(auteur(Prenom,A), L1) \= livres(auteur(AutrePrenom,B),L2),
    A @=< B,
    Prenom = AutrePrenom,
    write(Prenom),write(' '),write(A),write(' et '),write(AutrePrenom),write(' '),write(B),
    Prenom \= ''.

% auteurs_identiques_nom_complet/1 : cherche les auteurs ayant le même nom complet (prénom + nom de famille)
auteurs_identiques_nom_complet(NomComplet) :-
    livres(auteur(Prenom, Nom), L1),
    livres(auteur(AutrePrenom, AutreNom), L2),
    livres(auteur(Prenom, Nom), L1) \= livres(auteur(AutrePrenom, AutreNom), L2),
    Prenom @=< AutrePrenom,
    NomComplet = Prenom + ' ' + Nom,
    AutreNomComplet = AutrePrenom + ' ' + AutreNom,
    NomComplet = AutreNomComplet.
% On pourra les faire egalement avec les deux predicat precedent par un
% est logique

% 2- La somme des prix des livres d'un auteur dont on précise le prénom
% et le nom.
somme_des_prix_des_livres(Prenom, Nom, S) :-
    livres(auteur(Prenom, Nom), L),
    la_somme_des_prix_des_livres(Prenom, Nom, L, S).

la_somme_des_prix_des_livres(_, _, [], 0).

la_somme_des_prix_des_livres(Prenom, Nom, [(_,Y)|Reste], So) :-
    la_somme_des_prix_des_livres(Prenom, Nom, Reste, S_bis),
    So is S_bis + Y.




% 3- Le nombre de livres d'un auteur dont on précise le prénom et le
% nom.
nombre_de_livres(Prenom, Nom, N):-
    livres(auteur(Prenom, Nom), L),
    nombre_element(L, N).
nombre_element([], 0).
nombre_element([_], 1).
nombre_element([_,_|T], N) :-
    nombre_element(T, N1),
    N is N1 + 2.

% 4- Le maximum des prix des livres d'un auteur dont on précise le
% prénom et le nom.

maximum_des_prix(Prenom, Nom, Max) :-
    livres(auteur(Prenom, Nom), L),
    maximum(L, Max).
maximum([],_X) :- fail.
maximum([(_, Prix)], Prix).
maximum([(_, Y)|Reste], Max) :-
    maximum(Reste, Max_bis),
    max1(Max_bis, Y,L),
    Max is L.
max1(X,Y,M) :-
    X#>Y, M is X;
    M is Y.

% 5- Les livres d'un auteur dont le prix est inférieur (ou supérieur) à
% un prix donné.
livres_inf_prix(Prenom, Nom, PrixMax, Livres) :-
    livres(auteur(Prenom, Nom), TousLesLivres),
    trouver_livres_inf_prix(TousLesLivres, PrixMax, Livres).

livres_sup_prix(Prenom, Nom, PrixMin, Livres) :-
    livres(auteur(Prenom, Nom), TousLesLivres),
    trouver_livres_sup_prix(TousLesLivres, PrixMin, Livres).

trouver_livres_inf_prix([], _, []).

trouver_livres_inf_prix([(Titre, Prix)|Reste], PrixMax, [(Titre, Prix)|LivresInfPrix]) :-
    Prix =< PrixMax,
    trouver_livres_inf_prix(Reste, PrixMax, LivresInfPrix).
trouver_livres_inf_prix([(_, Prix)|Reste], PrixMax, LivresInfPrix) :-
    Prix > PrixMax,
    trouver_livres_inf_prix(Reste, PrixMax, LivresInfPrix).

trouver_livres_sup_prix([], _, []).
trouver_livres_sup_prix([(Titre, Prix)|Reste], PrixMin, [(Titre, Prix)|LivresSupPrix]) :-
    Prix >= PrixMin,
    trouver_livres_sup_prix(Reste, PrixMin, LivresSupPrix).
trouver_livres_sup_prix([(_, Prix)|Reste], PrixMin, LivresSupPrix) :-
    Prix < PrixMin,
    trouver_livres_sup_prix(Reste, PrixMin, LivresSupPrix).

% 6- Les titres des livres dont le prix = un certain prix donné en paramètre.
titres_prix(Prix) :-
    findall(Titre, (livres(auteur(_, _), L), member((Titre, Prix), L)), Titres),
    write(Titres).

% 7- La moyenne des prix des livres d'un auteur.
moyenne_prix_auteur(Prenom, Nom, Moyenne) :-
    livres(auteur(Prenom, Nom), L),
    length(L, N),
    N > 0,
    somme_prix(L, Somme),
    Moyenne is Somme / N.

somme_prix([], 0).
somme_prix([(_,P)|L], Somme) :-
    somme_prix(L, S),
    Somme is S + P.

% 8- Les auteurs ayant écrit un titre identique
auteurs_ayant_ecrit_titre_identique :-
    livres(auteur(P1,N1),L1),
    livres(auteur(P2,N2),L2),
    dif(livres(auteur(P1,N1),L1),livres(auteur(P2,N2),L2)),
    P1 @=< P2,
    member((Titre,_),L1),
    member((Titre,_),L2),
    format('~w ~w ont un titre commun dont le nom "~w".',[auteur(P1,N1),auteur(P2,N2),Titre]).



%9-1 L'auteur + le titre le plus cher (prix maximum de tous les livres)

livre_moins_cher_et_livre_plus_cher :-
    findall((Auteur, Titre, Prix), (livres(auteur(Nom, Prenom), Livres), member((Titre, Prix), Livres), Auteur = (Prenom, Nom)), LivresAuteurs),
    minimum3(LivresAuteurs, (Auteur1, Titre1, Prix1)),
    maximum3(LivresAuteurs, (Auteur2, Titre2, Prix2)),

    format('Le livre le moins cher de toute la base de données est "~w" de ~w, qui coûte ~w euros.~n', [Titre1, Auteur1, Prix1]),
    format('Le livre le moins cher de toute la base de données est "~w" de ~w ~w, qui coûte ~w euros.~n', [Titre2, Auteur2, Prix2]).


minimum3([], _) :- fail.
minimum3([(Auteur, Titre, Prix)], (Auteur, Titre, Prix)).
minimum3([(Auteur, Titre, Prix)|Reste], (AuteurM, TitreM, PrixM)) :-
    minimum3(Reste, (AuteurM_bis, TitreM_bis, PrixM_bis)),
    Prix =< PrixM_bis,
    (AuteurM, TitreM, PrixM) = (Auteur, Titre, Prix);
    minimum3(Reste, (AuteurM_bis, TitreM_bis, PrixM_bis)),
    (AuteurM, TitreM, PrixM) = (AuteurM_bis, TitreM_bis, PrixM_bis).


maximum3([], _) :- fail.
maximum3([(Auteur, Titre, Prix)], (Auteur, Titre, Prix)).
maximum3([(Auteur, Titre, Prix)|Reste], (AuteurM, TitreM, PrixM)) :-
    maximum3(Reste, (AuteurM_bis, TitreM_bis, PrixM_bis)),
    Prix > PrixM_bis,
    (AuteurM, TitreM, PrixM) = (Auteur, Titre, Prix);
    maximum3(Reste, (AuteurM_bis, TitreM_bis, PrixM_bis)),
    (AuteurM, TitreM, PrixM) = (AuteurM_bis, TitreM_bis, PrixM_bis).
