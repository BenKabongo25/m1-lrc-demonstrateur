/* 
SORBONNE UNIVERSITE
MASTER DAC, 2022-2023

LOGIQUE ET REPRESENTATION DES CONNAISSANCES
Démonstrateur de proposition

Ben KABONGO B.  21116436
*/


/* ======================================= UTILS =======================================  */

/* forme normale négative */
nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)):- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),X):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).


/* concatenation */
concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

/* suppression élément d'une liste */
enleve(X,[X|L],L) :-!.
enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

/* génère un nom de concept */
compteur(1).

genere(Nom) :- 
    compteur(V),nombre(V,L1),
    concat([105,110,115,116],L1,L2),
    V1 is V+1,
    dynamic(compteur/1),
    retract(compteur(V)),
    dynamic(compteur/1),
    assert(compteur(V1)),nl,nl,nl,
    name(Nom,L2).

nombre(0,[]).
nombre(X,L1) :-
    R is (X mod 10),
    Q is ((X-R)//10),
    chiffre_car(R,R1),
    char_code(R1,R2),
    nombre(Q,L),
    concat(L,[R2],L1).

chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').


/* liste des concepts */
allConcepts(L) :- 
    setof(X, cnamea(X), L1), 
    setof(Y, cnamena(Y), L2), 
    concat(L1, L2, L).

/* vérifie si X est une instance d'un concept (atomique ou complexe) */
isAtomicConcept(X) :- cnamea(X).
isComplexConcept(X) :- cnamena(X).
isConcept(X) :- cnamea(X).
isConcept(X) :- cnamena(X).

/* vérifie si X est une instance d'un concept existant */
isInstanceOfConcept(X) :- inst(X, Con), isConcept(Con).

/* liste des instances */
allInstances(L) :- setof(X, iname(X), L).

/* vérifie si X est bien une instance existante */
isInstance(X) :- iname(X).

/* liste des roles */
allRules(L) :- setof(X, rname(X), L).

/* vérifie si X est bien un rôle existant */
isRule(X) :- rname(X).



/* ======================================= PARTIE 1 =======================================  */

/* vérifie la syntaxe et la sémantique */
concept(X) :- isConcept(X).
concept(not(X)) :- concept(X).
concept(or(A, B)) :- concept(A), concept(B).
concept(and(A, B)) :- concept(A), concept(B).
concept(some(R, C)) :- isRule(R), concept(C).
concept(all(R, C)) :- isRule(R), concept(C).

concept((I, C)) :- isInstance(I), isConcept(C).
concept((I1, I2, R)) :- isInstance(I1), isInstance(I2), isRule(R).


/* vérifie si un concept fait appel à lui-même dans sa définition */
isIn(X, X).
isIn(X, C) :- equiv(C, Y), isIn(X, Y).
isIn(X, not(C)) :- isIn(X, C).
isIn(X, some(_, C)) :- isIn(X, C).
isIn(X, all(_, C)) :- isIn(X, C).
isIn(X, and(C, _)) :- isIn(X, C).
isIn(X, and(_, C)) :- isIn(X, C).
isIn(X, or(C, _)) :- isIn(X, C).
isIn(X, or(_, C)) :- isIn(X, C).

/* vérifie si un concept est auto-référent */
autoref(X) :- equiv(X, Y), isIn(X, Y).


/* remplace une expression complexe par des concepts atomiques sous forme normale négative */
atomize(C, C) :- isAtomicConcept(C).
atomize(C, NC) :- equiv(C, Y), atomize(Y, Z), nnf(Z, NC).
atomize(not(C), NC) :- atomize(C, NNC), nnf(not(NNC), NC).
atomize(some(R, C), NC) :- atomize(C, NNC), nnf(some(R, NNC), NC).
atomize(all(R, C), NC) :- atomize(C, NNC), nnf(all(R, NNC), NC).
atomize(and(C1, C2), NC) :- atomize(C1, NNC1), atomize(C2, NNC2), nnf(and(NNC1, NNC2), NC).
atomize(or(C1, C2), NC) :- atomize(C1, NNC1), atomize(C2, NNC2), nnf(or(NNC1, NNC2), NC).


/* traitement TBox */
/* convertit la TBox en liste de concepts traités */
traitement_Tbox(TBox) :- 
    concept(anything),
    concept(nothing),
    findall(X, equiv(X, _), L), element_TBox(L, TBox).

element_TBox([], []).
element_TBox([X|_], _) :- autoref(X), nl, write('ERREUR : concept auto-référent : '), write(X), halt(-1).
element_TBox([X|L], [(X,Y)|R]) :- atomize(X, Y), element_TBox(L, R).


/* traitement ABox */
/* traitement des instances */
traitement_i_Abox(Abi) :- findall((I, C), inst(I, C), L), element_i_Abox(L, Abi).

element_i_Abox([], []).
element_i_Abox([(I, C)|L], [(I, NC)|R]):- atomize(C, NC), element_i_Abox(L, R).

/* traitement des rôles */
traitement_r_Abox(Abr) :- findall((I1, I2, R), instR(I1, I2, R), Abr).

/* Abox sous forme des deux listes : instances et rôles */
traitement_Abox(Abi, Abr) :- traitement_i_Abox(Abi), traitement_r_Abox(Abr).


/* première étape */
premiere_etape(TBox, Abi, Abr) :- traitement_Tbox(TBox), traitement_Abox(Abi, Abr).


/* ======================================= PARTIE 2 =======================================  */

saisie_identificateur(I) :-
    nl, write('Identificateur :'),
    nl, read(I).

saisie_concept(C) :-
    nl, write('Concept :'),
    nl, read(C), concept(C).

saisie_concept(C) :-
    nl, write('Concept incorrect '),
    saisie_concept(C).

/* proposition de type 1 */
acquisition_prop_type1(Abi, Abe, _) :- 
    nl, write('Veuillez renseigner un identificateur et un concept :'),
    saisie_identificateur(I),
    saisie_concept(C),
    atomize(not(C), NC),
    concat(Abi, [(I, NC)], Abe).

/* proposition de type 2 */
acquisition_prop_type2(Abi, Abe, _) :- 
    nl, write('Veuillez renseigner deux concepts :'),
    saisie_concept(C1),
    saisie_concept(C2),
    atomize(and(C1, C2), NC),
    genere(A),
    concat(Abi, [(A, NC)], Abe).


suite(1, Abi, Abe, Tbox) :- acquisition_prop_type1(Abi, Abe, Tbox),!.
suite(2, Abi, Abe, Tbox) :- acquisition_prop_type2(Abi, Abe, Tbox),!.
suite(_, Abi, Abe, Tbox) :- nl,write('Cette reponse est incorrecte.'),nl,
    saisie_et_traitement_prop_a_demontrer(Abi, Abe, Tbox).

saisie_et_traitement_prop_a_demontrer(Abi, Abe, Tbox) :-
    nl, write('Entrez le numéro du type de proposition que vous voulez demontrer :'),
    nl, write('1 : Une instance donnée appartient a un concept donne.'),
    nl, write('2 : Deux concepts n\'ont pas d\'éléments en commun (ils ont une intersection vide).'),
    nl, read(R), suite(R, Abi, Abe, Tbox).

deuxieme_etape(Abi, Abe, Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi, Abe, Tbox).

check_deuxieme_etape(Abi, Abe, Tbox) :-
    premiere_etape(Tbox, Abi, _),
    deuxieme_etape(Abi, Abe, Tbox).

/* ======================================= PARTIE 3 =======================================  */


/* tri de la ABox en fonction de la forme des assertions */
tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls) :-
    findall((I, some(R, C)), member((I, some(R, C)), Abi), Lie),
    findall((I, all(R, C)), member((I, all(R, C)), Abi), Lpt),
    findall((I, and(C1, C2)), member((I, and(C1, C2)), Abi), Li),
    findall((I, or(C1, C2)), member((I, or(C1, C2)), Abi), Lu),
    findall((I, not(C)), member((I, not(C)), Abi), Lo),
    findall((I, C), (member((I, C), Abi), isAtomicConcept(C)), L),
    concat(L, Lo, Ls).
    
check_tri(Abi, Lie, Lpt, Li, Lu, Ls) :- 
    traitement_i_Abox(Abi), 
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).


/* ajoute un concept à la liste adéquate */
/*
evolue((I,           C),   _,   _,  _,  _, Ls,    _,    _,   _,   _, Ls1) :- isAtomicConcept(C), concat([(I, C)], Ls, Ls1).
evolue((I,      not(C)),   _,   _,  _,  _, Ls,    _,    _,   _,   _, Ls1) :- concat([(I,      not(C))],  Ls,  Ls1).
evolue((I, and(C1, C2)),   _,   _, Li,  _,  _,    _,    _, Li1,   _,   _) :- concat([(I, and(C1, C2))],  Li,  Li1).
evolue((I,  or(C1, C2)),   _,   _,  _, Lu,  _,    _,    _,   _, Lu1,   _) :- concat([(I,  or(C1, C2))],  Lu,  Lu1).
evolue((I,  some(R, C)), Lie,   _,  _,  _,  _, Lie1,    _,   _,   _,   _) :- concat([(I,  some(R, C))], Lie, Lie1).
evolue((I,   all(R, C)),   _, Lpt,  _,  _,  _,    _, Lpt1,   _,   _,   _) :- concat([(I,   all(R, C))], Lpt, Lpt1).
*/
evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, [(I, some(R,C)) | Lie], Lpt, Li, Lu, Ls).
evolue((I,  all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I, all(R, C)) | Lpt], Li, Lu, Ls).
evolue((I,  and(A, B)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I, and(A, B)) | Li], Lu, Ls).
evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I, or(C1, C2)) | Lu], Ls).
evolue((I,          C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, C) | Ls]).


/* affichage d'évolution des assertions */

/* affiche un concept sans paranthèse si il est atomique, avec sinon */
write_bracket_concept(C) :- isAtomicConcept(C), write(C).
write_bracket_concept(C) :- write('('), write_concept(C), write(')').

/* affichage d'un concept */
write_concept(C) :- isAtomicConcept(C), write(C).
write_concept(C) :- equiv(C, EC), write_concept(EC).
write_concept(not(C)) :- write('¬'), write_bracket_concept(C).
write_concept(some(R, C)) :- write('∃'), write(R), write('.'), write_bracket_concept(C).
write_concept(all(R, C)) :- write('∀'), write(R), write('.'), write_bracket_concept(C).
write_concept(and(C1, C2)) :- write_bracket_concept(C1), write(' ⊓ '), write_bracket_concept(C2).
write_concept(or(C1, C2)) :- write_bracket_concept(C1), write(' ⊔ '), write_bracket_concept(C2).


/* affichage d'une assertion de concept */
write_concept_assert((I, C)) :- write(I), write(' : '), write_concept(C).

/* affichage d'une liste d'assertions de concepts */
write_concept_assert_list([]).
write_concept_assert_list([(I, C) | L]) :-
    write('      '),
    write_concept_assert((I, C)),
    nl,
    write_concept_assert_list(L).


/* affichage d'une assertion de rôle */
write_rule_assert((I1, I2, R)) :- write('< '), write(I1), write(' , '), write(I2), write(' > : '), write(R).

/* affichage d'une liste assertions de rôles */
write_rule_assert_list([]).
write_rule_assert_list([(I1, I2, R) | L]) :-
    write('      '),
    write_rule_assert((I1, I2, R)),
    nl,
    write_rule_assert_list(L).


check_write_concept_assert_list :-
    traitement_i_Abox(Abi), write_concept_assert_list(Abi).

check_write_rule_assert_list :-
    traitement_r_Abox(Abr), write_rule_assert_list(Abr).


/* affichage de l'évolution */
affiche_evolution_Abox(Lie1, Lpt1, Li1, Lu1, Ls1, Abr1, Lie2, Lpt2, Li2, Lu2, Ls2, Abr2) :-
    !,
    nl, write('      ABox  : '),
    nl, write('      ========================= (1)'),
    nl,
    write_concept_assert_list(Lie1),
    write_concept_assert_list(Lpt1),
    write_concept_assert_list(Li1),
    write_concept_assert_list(Lu1),
    write_concept_assert_list(Ls1),
    nl, write_rule_assert_list(Abr1),
    nl, write('      ========================= (2)'),
    nl,
    write_concept_assert_list(Lie2),
    write_concept_assert_list(Lpt2),
    write_concept_assert_list(Li2),
    write_concept_assert_list(Lu2),
    write_concept_assert_list(Ls2),
    nl, write_rule_assert_list(Abr2)
    .


/* test clash */
test_clash([(A, C) | Ls]) :- 
    nnf(not(C), NNC),
    member((A, NNC), Ls),
    nl, write('      ========================= CLASH ! : '), 
    nl, write('      ('),  write(A), write(' : '), write_concept(C), write(') et ('), 
    write(A), write(' : '), write_concept(not(C)), write(') trouvés ').
test_clash([(_, _) | Ls]) :- test_clash(Ls).


/* Règle il existe  a : ∃R.C ->  (a, b) : R et b : C */
complete_some([(A, some(R, C)) | Lie], Lpt, Li, Lu, Ls, Abr) :-
    genere(B),
    evolue((B, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    concat([(A, B, R)], Abr, Abr1),
    nl, write('==================================== Règle ∃ ===================================='),
    nl, affiche_evolution_Abox([(A, some(R, C)) | Lie], Lpt, Li, Lu, Ls, Abr, Lie1, Lpt1, Li1, Lu1, Ls1, Abr1),
    !, resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr1).
complete_some([], Lpt, Li, Lu, Ls, Abr) :- !, transformation_and([], Lpt, Li, Lu, Ls, Abr).


/* Règle and   a : C ⊓ D -> a : C et a : D */
transformation_and(Lie, Lpt, [(A, and(C1, C2)) | Li], Lu, Ls, Abr) :-
    evolue((A, C1),  Lie,  Lpt,  Li,  Lu,  Ls, Lie0, Lpt0, Li0, Lu0, Ls0),
    evolue((A, C2), Lie0, Lpt0, Li0, Lu0, Ls0, Lie1, Lpt1, Li1, Lu1, Ls1),
    nl, write('==================================== Règle ⊓ ===================================='),
    nl, affiche_evolution_Abox(Lie, Lpt, [(A, and(C1, C2)) | Li], Lu, Ls, Abr, Lie1, Lpt1, Li1, Lu1, Ls1, Abr),
    !, resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).
transformation_and(Lie, Lpt, [], Lu, Ls, Abr) :- !, deduction_all(Lie, Lpt, [], Lu, Ls, Abr).


/* Règle pour tout a : ∀R.C et 〈a, b〉: R -> b : C */
deduction_all(Lie, [(A, all(R, _)) | Lpt], Li, Lu, Ls, Abr) :-
    not(member((A, _, R), Abr)),
    !, resolution(Lie, Lpt, Li, Lu, Ls, Abr).
deduction_all(Lie, [(A, all(R, C)) | Lpt], Li, Lu, Ls, Abr) :- 
    member((A, B, R), Abr),
    enleve((A, B, R), Abr, Abr1),
    evolue((B, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    nl, write('==================================== Règle ∀ ===================================='),
    nl, affiche_evolution_Abox(Lie, [(A, all(R, C)) | Lpt], Li, Lu, Ls, Abr, Lie1, Lpt1, Li1, Lu1, Ls1, Abr1),
    !, deduction_all(Lie1, [(A, all(R, C)) | Lpt1], Li1, Lu1, Ls1, Abr1).
deduction_all(Lie, [], Li, Lu, Ls, Abr) :- !, transformation_or(Lie, [], Li, Lu, Ls, Abr).


/* Règle or a : C ⊔ D -> a : C (noeud 1) et a : D (noeud 2) */
transformation_or(Lie, Lpt, Li, [(A, or(C1, C2)) | Lu], Ls, Abr) :-    
    evolue((A, C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    nl, write('==================================== Règle ⊔ (1) ===================================='),
    nl, affiche_evolution_Abox(Lie, Lpt, Li, [(A, or(C1, C2)) | Lu], Ls, Abr, Lie1, Lpt1, Li1, Lu1, Ls1, Abr),
    !, resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr),
    evolue((A, C2), Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    nl, write('==================================== Règle ⊔ (2) ===================================='),
    nl, affiche_evolution_Abox(Lie, Lpt, Li, [(A, or(C1, C2)) | Lu], Ls, Abr, Lie2, Lpt2, Li2, Lu2, Ls2, Abr),
    !, resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr).


/* résolution */
resolution(Lie, Lpt, Li, Lu, Ls, Abr) :- 
    test_clash(Ls), 
    nl, affiche_evolution_Abox([], [], [], [], [], [], Lie, Lpt, Li, Lu, Ls, Abr), !.
resolution(Lie, Lpt, Li, Lu, Ls, Abr) :- 
    not(test_clash(Ls)),
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr).



/* troisième étape */
troisieme_etape(Abe, Abr) :-
    tri_Abox(Abe, Lie, Lpt, Li, Lu, Ls),
    nl, write('============================================================================= ABox =='),
    nl, affiche_evolution_Abox([], [], [], [], [], [], Lie, Lpt, Li, Lu, Ls, Abr),
    nl, write('======================================================================== Résoluion =='),
    nl, !, resolution(Lie, Lpt, Li, Lu, Ls, Abr),
    nl, write('=============================================== Proposition correctement démontrée ==').


/* ======================================= MAIN =======================================  */

intro :- 
    nl, write('===================================================== DEMONSTRATEUR DE PROPOSITION '),
    nl, write('Démonstrateur basé sur l’algorithme des tableaux pour la logique de description ALC'),
    nl, write('SORBONNE UNIVERSITE, MASTER DAC, 2022-2023, @Ben KABONGO'),
    nl, write('==================================================================================='), nl.


/* programme principal */
programme :- intro,
    premiere_etape(Tbox,Abi,Abr),
    deuxieme_etape(Abi,Abe,Tbox),
    troisieme_etape(Abe,Abr).

