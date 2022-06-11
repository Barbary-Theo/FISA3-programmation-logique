/* Prédicat qui pourra servir afin d'afficher de manière plus lisible notre echiquier avec le chemin */

affiche_echiquier(E) :- affiche_echiquier(E, 1).
affiche_echiquier([], _).
affiche_echiquier([L|R], I) :- longueur_echiquier(Len), Reste is I mod Len, Reste == 0, write(L),write(', '), nl, NI is I + 1, affiche_echiquier(R, NI), !.
affiche_echiquier([L|R], I) :- write(L), write(','), NI is I + 1, affiche_echiquier(R, NI).

/* Longueur d'un côté de l'échiquier */

longueur_echiquier(5).

/* Chemins possibles pour le cavalier */

get_coords(I, X, Y) :- longueur_echiquier(Len), Y is I // Len, X is I mod Len.
set_coords(X, Y, I) :- X >= 0, Y >= 0, longueur_echiquier(Len), X < Len, Y < Len, I is (Y * Len) + X.

deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc - 2, XDest is XSrc - 1, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc - 2, XDest is XSrc + 1, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc - 1, XDest is XSrc - 2, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc - 1, XDest is XSrc + 2, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc + 1, XDest is XSrc - 2, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc + 1, XDest is XSrc + 2, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc + 2, XDest is XSrc - 1, set_coords(XDest, YDest, Dest).
deplacement_cheval(Src, Dest) :- get_coords(Src, XSrc, YSrc), YDest is YSrc + 2, XDest is XSrc + 1, set_coords(XDest, YDest, Dest).

/* Mettre à jour l'echiquier en incluant une nouvelle case dans son chemin (produit une nouvelle liste) */

modifier([_|R], 0, Val, [Val|R]).
modifier([L|R], Index, Val, [L|R2]) :- NewIndex is Index - 1, modifier(R, NewIndex, Val, R2).

/* Initialisation d'un echiquier */

init_echiquier([], _, 0).
init_echiquier([1|R], 0, Len) :- NewLen is Len - 1, init_echiquier(R, -1, NewLen), !.
init_echiquier([0|R], Depart, Len) :- NewLen is Len - 1, NewDepart is Depart - 1, init_echiquier(R, NewDepart, NewLen).

/* Recherche d'un chemin , le 25 pour la condition d'arrêt dépend de la longueur des côtés de l'échiquier. Si le côté est égal à N, alors cette valeur doit être changée en N*N */

recherche_chemin(Echiquier, _, 25, Echiquier) :- affiche_echiquier(Echiquier).
recherche_chemin(TmpEchiquier, Depart, Val, Echiquier) :- deplacement_cheval(Depart, Deplacement), nth0(Deplacement, TmpEchiquier, 0), NewVal is Val + 1, modifier(TmpEchiquier, Deplacement, NewVal, NewTmpEchiquier), recherche_chemin(NewTmpEchiquier, Deplacement, NewVal, Echiquier).

/* Prédicat general */

probleme_cavalier(Echiquier, Depart) :- longueur_echiquier(Len), NbCases is Len * Len, init_echiquier(TmpEchiquier, Depart, NbCases), recherche_chemin(TmpEchiquier, Depart, 1, Echiquier).