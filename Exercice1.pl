:- dynamic(professeur/4).
:- dynamic(eleve/4).
:- dynamic(salle/4).
:- dynamic(matiere/5).
:- dynamic(groupe/4).
:- dynamic(seance/6).
:- dynamic(classe/7).


/*
   _________________________________________
    Question 1 ~~> insertion et suppression
*/
insererProfesseur(LASTNAME,FIRSTNAME,SUBJECT,ID):-
    not(professeur(_,_,_,ID)) ,
    assert(professeur(LASTNAME,FIRSTNAME,SUBJECT,ID)).

insererEleve(LASTNAME,FIRSTNAME,LEVEL,ID):-
    not(eleve(_,_,_,ID)),
    assert(eleve(LASTNAME,FIRSTNAME,LEVEL,ID)).

insererSalle(NAME,CAPACITY,TYPE,ID):-
    not(salle(_,_,_,ID)),
    assert(salle(NAME,CAPACITY,TYPE,ID)).

insererMatiere(NAME,SUBJECT,TYPE,LEVEL,ID):-
    not(matiere(_,_,_,_,ID)),
    assert(matiere(NAME,SUBJECT,TYPE,LEVEL,ID)).

insererGroupe(NAME,LEVEL,LISTSTUDENT,ID):-
    not(groupe(_,_,_,ID)),
    assert(groupe(NAME,LEVEL,LISTSTUDENT,ID)).

insererSeance(SUBJECT,TEACHER,GROUP,ROOM,CRENCH,ID):-
    not(seance(_,_,_,_,_,ID)),
    professeur(_,_,_,TEACHER),
    groupe(_,_,_,GROUP),
    salle(_,_,_,ROOM),
    matiere(_,_,_,_,SUBJECT),
    assert(seance(SUBJECT,TEACHER,GROUP,ROOM,CRENCH,ID)).


supprimerProfesseur(ID):-
    retract(professeur(_,_,_,ID)).

supprimerEleve(ID):-
    retract(eleve(_,_,_,ID)).

supprimerSalle(ID):-
    retract(salle(_,_,_,ID)).

supprimerMatiere(ID):-
    retract(matiere(_,_,_,_,ID)).

supprimerGroupe(ID):-
    retract(groupe(_,_,_,ID)).

supprimerSeance(ID):-
    retract(seance(_,_,_,_,_,ID)).


/*
   ________________________________________________
    Question 2 ~~> ajout d un élève dans un groupe
*/
ajoutListe(X,L, L1) :- L1 =[X|L].


isInList(X,[X|_]).
isInList(X,[_|R]):- isInList(X,R).

nbEleInList([], 0).
nbEleInList([_|L1], Nb):-
    nbEleInList(L1, N),
    Nb is N + 1.


switch(X, [Val:Goal|Cases]) :-
    ( X = Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).


eleveInAllGroupe(ID):-
    groupe(_,_,STUDENTS,_),
    isInList(ID,STUDENTS).


ajouterDansListe(ELE,LIST,[ELE|LIST]).


ajouterEleveGroupe(IDSTUDENT,IDGROUP):-
    groupe(NAME,LEVEL,LISTSTUDENT,IDGROUP),
    not(isInList(IDSTUDENT,LISTSTUDENT)),
    not(eleveInAllGroupe(IDSTUDENT)),
    supprimerGroupe(IDGROUP),
    insererGroupe(NAME,LEVEL,[IDSTUDENT|LISTSTUDENT],IDGROUP).

/*
   _____________________________________________________________
    Question 3 ~~> séance avec un problème de capacité de salle
*/
seanceProblemeCapacite():-
    seance(_,_,GROUP,ROOM,_,_),
    groupe(_,_,LISTEELEVE,GROUP),
    salle(_,CAPACITY,_,ROOM),
    nbEleInList(LISTEELEVE,NBELEMENT),
    CAPACITY < NBELEMENT.

/*
   __________________________________________________________________________________
    Question 4 ~~> récupérer les séances d un professeur, d une salle ou d un groupe
*/
getSeanceProf(IDTEACHER, L):-
    findall(IDSEANCE, seance(_, IDTEACHER, _, _, _, IDSEANCE), L).

getSeanceEleve(IDSTUDENT, L):-
    groupe(_,_,LISTSTUDENT,IDGROUP),
    member(IDSTUDENT, LISTSTUDENT),
    findall(IDSEANCE, seance(_, _, IDGROUP, _, _, IDSEANCE), L).

getSeanceSalle(IDROOM, L):-
    findall(IDSEANCE, seance(_, _, _, IDROOM, _, IDSEANCE), L).

getSeanceGroupe(IDGROUP, L):-
    findall(IDSEANCE, seance(_, _, IDGROUP, _, _, IDSEANCE), L).

/*
   ______________________________________________________
    Question 5 ~~> récupérer le jour d un créneau donnée
*/
getJourSeance(IDSEANCE):-
    seance(_,_,_,_,CRENCH,IDSEANCE),
    sub_atom(CRENCH, _, 1, 1, C),
    switch(C,[
           '1' : write('lundi '),
           '2' : write('mardi '),
           '3' : write('mercredi '),
           '4' : write('jeudi '),
           '5' : write('vendredi '),
           '6' : write('samedi '),
           '7' : write('dimanche '),
           _ : write('inconnu ')]).


/*
   ________________________________________________________
    Question 6 ~~> récupérer l horaire d un créneau donnée
*/
getHoraireSeance(IDSEANCE):-
    seance(_,_,_,_,CRENCH,IDSEANCE),
    sub_atom(CRENCH, _, 1, 0, C),
    switch(C,[
           '1' : write('8h-10h '),
           '2' : write('10h-12h '),
           '3' : write('14h-16h '),
           '4' : write('16h-18h '),
               _ : write('inconnu ')]).


/*
   ______________________________________________________________
    Question 7 ~~> vérifier si les deux séances sont en conflits
*/
verifSeanceConflit(IDFIRSTSEANCE,IDSECONDSEANCE):-
    seance(_,TEACHERFIRST,GROUPFIRST,ROOMFIRST,CRENCHFIRST,IDFIRSTSEANCE),
    seance(_,TEACHERSECOND,GROUPSECOND,ROOMSECOND,CRENCHSECOND,IDSECONDSEANCE),
    TEACHERFIRST \= TEACHERSECOND,
    GROUPFIRST \= GROUPSECOND,
    ROOMFIRST \= ROOMSECOND,
    CRENCHFIRST \= CRENCHSECOND.


/*
   _________________________________________________________________________________
    Question 8 ~~> récupérer le jour d un créneau donnée
*/
afficherProf(IDPROF):-
    professeur(LASTNAME,FIRSTNAME,_,IDPROF),
    format('~w ~w ~n', [LASTNAME, FIRSTNAME]),
    seance(SUBJECT, IDPROF, GROUP, ROOM, _ , IDSEANCE),
    salle(ROOMNAME, _, _, ROOM),
    groupe(NAME, _, _, GROUP),
    matiere(SUBJECTNAME,_,_,_,SUBJECT),
    getJourSeance(IDSEANCE),
    getHoraireSeance(IDSEANCE),
    format(' -> matière : ~w , salle : ~w , groupe : ~w ~n', [SUBJECTNAME, ROOMNAME, NAME]).

afficherEleve(IDSTUDENT):-
    eleve(LASTNAME, FIRSTNAME, _, IDSTUDENT),
    format('- ~w ~w ~n', [LASTNAME, FIRSTNAME]),
    groupe(_,_,LISTSTUDENT,IDGROUP),
    member(IDSTUDENT, LISTSTUDENT),
    seance(SUBJECT, IDPROF, IDGROUP, ROOM, _, IDSEANCE),
    professeur(TEACHNAME, _, _, IDPROF),
    salle(ROOMNAME, _, _, ROOM),
    matiere(SUBJECTNAME,_,_,_,SUBJECT),
    getJourSeance(IDSEANCE),
    getHoraireSeance(IDSEANCE),
    format(' -> matière : ~w , salle : ~w , professeur : ~w ~n', [SUBJECTNAME, ROOMNAME, TEACHNAME]).

afficherSalle(IDROOM):-
    salle(NAMEROOM,_,_,IDROOM),
    format('~w ~n', [NAMEROOM]),
    seance(SUBJECT, IDPROF, IDGROUP, IDROOM, _ , IDSEANCE),
    groupe(GROUPNAME,_,_,IDGROUP),
    professeur(TEACHNAME, _, _, IDPROF),
    matiere(SUBJECTNAME,_,_,_,SUBJECT),
    getJourSeance(IDSEANCE),
    getHoraireSeance(IDSEANCE),
    format(' -> matière : ~w , Groupe : ~w , professeur : ~w ~n', [SUBJECTNAME, GROUPNAME, TEACHNAME]).
    
    
/*
   ________________________________________________
    Question 9 ~~> Calcule de la charge de travail
*/
chargeProf(IDTEACHER, NB):-
    getSeanceProf(IDTEACHER, L),
    nbEleInList(L, NB).

chargeEleve(IDSTUDENT, NB):-
    getSeanceEleve(IDSTUDENT, L),
    nbEleInList(L, NB).

chargeSalle(IDROOM, NB):-
    getSeanceSalle(IDROOM, L),
    nbEleInList(L, NB).

/*
   ________________________________________________
    Question 10 ~~> Ajout de la structure classe
*/
insererClasse(NAME, LEVEL, IDGROUP, LISTTEACHER, LISTSUBJECT, LISTSEANCE, ID):-
    not(classe(_,_,_,_,_,_,ID)),
    groupe(_,_,_,IDGROUP),
    assert(classe(NAME,LEVEL,IDGROUP,LISTTEACHER,LISTSUBJECT,LISTSEANCE,ID)).

supprimerClasse(IDCLASSE):-
    retract(classe(_,_,_,_,_,_,IDCLASSE)).



/*
   _____________________________________________________________________________
    Question 11 ~~> Vérfication de nombre de séance par matière pour une classe
*/

verifSeanceClasse(IDCLASSE):-
    classe(_,_,IDGROUP,_,SUBJECTS,_,IDCLASSE),
    forall(
        member(SUBJECT, SUBJECTS),
        (
         findall(IDSEANCE, seance(SUBJECT,_,IDGROUP,_,_,IDSEANCE), L),
         nbEleInList(L,NB),
         NB = 2
       )
    ).


/*
   ______________________________________________
    Question 12 ~~> Ajout de la structure classe
*/

verifAll(_, _, _, _, LISTCLASS):-
    forall(
        member(IDCLASS, LISTCLASS),
        (
            verifSeanceClasse(IDCLASS)   
            /* verifSeanceConflit(SEANCE1, SEANCE2) */     
        )
    ),
    seanceProblemeCapacite().
    /* 
    seance(_,_,_,_,_,IDSEANCE).
    verifSeanceConflit(SEANCE1, SEANCE2)
    */     


/*
   ________________________________________________________
    Question 13 ~~> Sauvegarde et écriture dans un fichier
*/

sauvegarderData() :-
    open('save.pl',write,File), 
    set_output(File),  
    listing(eleve), 
    listing(professeur), 
    listing(salle), 
    listing(matiere), 
    listing(groupe), 
    listing(seance), 
    listing(classe), 
    close(File).

recupererData() :-
    consult('save.pl').
