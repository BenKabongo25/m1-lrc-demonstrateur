/* 
SORBONNE UNIVERSITE
MASTER DAC, 2022-2023

LOGIQUE ET REPRESENTATION DES CONNAISSANCES
Démonstrateur de proposition

Ben KABONGO B.  21116436
*/


/* programme de test */

equiv(sculpteur,and(personne,some(aCree,sculpture))).
equiv(auteur,and(personne,some(aEcrit,livre))).
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).

cnamea(personne).
cnamea(livre).
cnamea(objet).
cnamea(sculpture).
cnamea(anything).
cnamea(nothing).

cnamena(auteur).
cnamena(editeur).
cnamena(sculpteur).
cnamena(parent).

iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).
iname(ben).

rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).

inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).
inst(ben,sculpteur).

instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).
