Ce que j'ai fait :  -Implémenter la partie obligatoire
                    -Ajout d'un critère "boule isolée" qui stoppe le parcours si une boule devient inaccessible quel que soit le mouvement
                    -Amélioration de l'affichage des boules (petit effet de lumière)(modification de la fonction draw_ball)
                    -Génération aléatoire : Génération aléatoire puis recherche d'une solution (peut parfois être très long pour 15 boules)
                    -Sauvegarde et chargement de configuration depuis un fichier

Choix d'implémentation :
1) TYPES:
    - ball : Un record contenant une position et un identifiant tous deux non mutables. Seule la position est utile mais lorsqu'elle était modifiée la boule devenait incorrecte dans les multiples instances de game. J'ai donc rendu la position non mutable. L'identifiant à été ajouté après pour retenir l'information de l'identité de la boule pour obtenir un affichage correct. Les nouveaux id sont donnés par une variable globale qui est incrémentée à chaque création de boule.
    - game : Un record contenant deux hashtable de listes de boules. L'une contient les boules de même abscisse (la clef est l'abscisse commune), l'autre de même ordonnée. Les listes sont maintenues triées en fonction de la deuxième coordonnée (selon l'ordonnée pour les liste dans la table des abscisses, par abscisse pour l'autre). Cette structure permet une bonne complexité pour les fonctions moves et apply_moves. En effet il suffit de considérer les colonnes et lignes dont il est question et d'ignorer le reste. Les boules "adjacentes" (qui n'ont pas d'autres boules entre elles) se suivent dans les listes, il suffit donc de considérer les pairs de boules successives pour connaître les mouvements possibles. Cette définition de game permet de ne pas dépendre d'une taille de grille fixe (utile pour résoudre à partir de fichiers de configuration et plus extensible) et de ne jamais considérer des cases vides. La taille de game est proportionnelle au nombre de boules et non au nombre de cases. La rapidité des fonctions moves et apply_moves ainsi que la taille réduite de game permet une résolution plus rapide (bien que longue vu l'algorithme utilisé).

2) SAUVEGARDE:
    Les configurations sauvegardées sont sous la forme d'un fichier texte (.flg). La première ligne est le nombre de boules de la configuration (N) et les N lignes suivantes contiennent les coordonnées x et y de chaque boule séparées par un espace.
    Cela permet de connaître le nombre de boules à l'avance et de traiter une boule à la fois lors de la lecture du fichier (ligne par ligne).

3) FONCTIONS
    - apply_move : Extrait la liste de boules pertinentes relativement au déplacement. A l'origine apply_move devait mettre à jour l'une des coordonnées de chaque boule de la liste puis supprimer la dernière. Au vu du nouveau type pour ball, apply_move supprime la boule à déplacer puis en ajoute une nouvelle avec le même id à l'emplacement d'arrivée (un peu moins efficace).
    - moves : Pour chaque liste présente dans les hashtable de game, vérifie si une pair de boules successives a une distance supérieure à 1. Pour chaque boule, on ne considère que les directions dans lesquelles il y a d'autres boules (C'est immédiat il suffit de savoir si la boule a un successeur ou un prédécesseur dans sa liste) et on vérifie si le mouvment est valide (écart de plus de 1).
    - J'ai ajouté add_ball et rm_ball qui permettent respectivement d'ajouter une boule à un jeu et de la supprimer.
    - eq_ball compare simplement les id des boules
    - hash_find permet de rechercher dans une hashtable et de formater directement la valeur trouvée.
    - J'ai rajouté un argument optionnel à make_ball pour préciser si on veut un id particulier pour la boule nouvellement créée (id != 0)

    - loop : A chaque itération de loop, demande l'action au joueur, la compare avec les actions possibles et l'applique si possible

    - solve : Parcours en profondeur du graphe des configurations atteignables. Termine car le nombre de boules est strictement décroissant lorsqu'on progresse dans une partie et à partir de chaque configuration il n'y a qu'un nombre fini de mouvements possibles. On ne vérifie pas si une configuration a déjà été rencontrée. Vérifie à chaque fois le critère "check" indiquant si la configuration actuelle est insolvable ou potentialement résolvable.
    - check : Cherche si une boule s'est retrouvée isolée (ie : aucune suite de mouvements ne permettra de la déplacer) auquel cas il restera au moins deux boules à la fin -> pas de solution. Pour cela je découpe le plateau en quadrants centrés sur l'une des boules. Si trois de ces quadrants sont vides alors la boule est isolée. Pour cela je teste s'il existe ou non des boules au dessus,en dessous,à gauche et à droite de la boule considérées et s'il existe deux zones vides de la forme Gauche/Haut,Haut/Droite,Droite/Bas,Bas/Gauche alors on a en fait 3 quadrants vides.

Ce que je n'ai pas eu le temps d'implémenter:
Le critère d'arrêt de la "boule isolée" peut se généraliser en "zones isolées". Si l'on trouve un découpage en quadrants tel que deux quadrants opposés sont vides et les deux autres contiennent tous deux des boules alors les boules contenues dans deux quadrants différents ne pourront jamais interagir entre elles donc il restera au moins deux boules à la fin.

Boule isolée
o  o|           o|    
o o |           -|----
----|----        | o o
    | o          | o   

Zones isolées
o  o|           o|    
o o |           o|
----|----       -|----
    | o o        |o  o
    |  o         | o   