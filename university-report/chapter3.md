
# Regroupement de noms
L’outil de regroupement de noms fut le premier outil que j’ai implémenté au cours du projet. L’objectif principal était d’avoir un moyen de présenter les prénoms d’une autre façon que via une simple liste, pouvant donner lieu à des groupes pertinents (Christine, Kirsten, Kristen, prénoms tous féminins)… ou non (Alexandre, Alexandra, prénoms de genre différent).

## Notion de distance
Pour aborder cet algorithme, j’ai commencé par une implémentation basique se contentant de placer les noms donnés dans des groupes appelés “clusters” selon certains critères. Concrètement, voici ce que faisait ma première implémentation d’algorithme : pour chaque nom *A* à classer, on recherche un cluster dont le premier nom a une distance du nom *A* inférieure à une constante prédéfinie. Si aucun cluster n’a été trouvé, le prénom *A* forme un nouveau cluster.

J’évoque ci-dessus la notion de distance. De quoi s’agit-il exactement ? J’ai utilisé ici la **distance de Levenshtein**. Il s’agit d’une notion mathématique que nous utilisons dans notre cas pour évaluer le degré de similitude de deux chaînes de caractères. Concrètement, il s’agit d’une nombre d’opérations de caractère (substitution, ajout, suppression) pour transformer une chaîne en l’autre. Ainsi, pour passer du prénom *Alexandre* à *Alexandra*, il est nécessaire de subtituer la dernière lettre (*a* devient *e*, ou *e* devient *a*) ; la distance de Levenshtein vaut alors 1.

Cette implémentation s’avéra être une base convenable pour la suite, mais elle présentait bien évidemment plusieurs défauts. Tout d’abord, l’utilisation du premier nom du cluster n’est absolument pas représentatif du cluster. Ensuite, l’utilisation d’une constante prédéfinie résultera en des résultats hétérogènes pour des noms de longueur très variée. Il était alors plus judicieux d’utiliser une variable dépendant de la longueur des mots plutôt qu’une constante.

## Partitionnement en k-moyennes
La version finale de notre algorithme de classification est inspirée de celui de partitionnement en k-moyennes (ou *k-means* en anglais), que nous allons expliquer ici afin de mieux comprendre ce qui m’a mené à l’algorithme obtenu. Cet algorithme est utilisé pour classifier des données en un nombre prédéfinis de groupes selon une distance définie. Il est généralement nécessaire de transformer les données afin d’obtenir un problème purement géométrique (c’est-à-dire, uniquement des données sous forme numérique). Les clusters possèdent tous un centre qui représente la moyenne des données, et l’algorithme consiste en deux étapes : une étape d’**assignation**, et une étape de **réévaluation**.

Durant l’étape d’assignation, chaque donnée est placée dans le cluster dont elle est le plus proche ; on calcule pour cela la distance de la donnée au centre du cluster. Une fois toutes les données placées (et donc l’étape d’assignation terminée), on recalcule tous les centres des clusters selon la moyenne des données contenues dans chaque : il s’agit de l’étape de réévaluation. Enfin, dans le cas où des données ont changé de groupes durant l’étape d’assignation, on réassigne à nouveau toutes les données aux clusters, et on réévalue les centres, et ainsi de suite… jusqu’à qu’aucune donnée ne change de groupe. En résumé :

1. On définit des centres initiaux pour tous les clusters.
2. On assigne toutes les données à un cluster.
3. Si aucune donnée n’a changé de groupes, arrêter et sortir les clusters.
3. On réévalue les centres des clusters.
4. Retour en 2. 

## Version finale de l’algorithme
Dans notre cas, il n’était pas possible d’implémenter un algorithme en k-moyennes pour deux raisons :

* je ne souhaitais pas devoir spécifier le nombre de clusters au départ de l’algorithme, car il me semblait préférable de laisser l’application gérer automatiquement cela ;
* il n’est pas possible de réévaluer les centres de façon classique, puisque les données sont des chaînes de caractères et non des valeurs numériques.

Pour répondre au problème du nombre de clusters, il a suffit de reprendre la première approche algorithmique que j’avais implémentée. En effet, la condition “si la distance(nom, centre) est inférieure à une constante” permettant de savoir si un nom se retrouve dans un cluster ou en constitue un nouveau par lui-même, la nécessité de spécifier le nombre de clusters n’existe plus. On pourra alors moduler la constante afin de varier la tolérance de chaque cluster, et ainsi influencer le nombre de clusters à la sortie.

Concernant la question de réévaluation des centres, j’avais deux possibilités : générer des chaînes de caractères représentant le centre de chaque cluster, ou utiliser des pseudo-centres plutôt que des centres. C’est cette deuxième option que j’ai implémenté, la génération de chaînes de caractères semblant trop complexe pour être envisagée. Par conséquent, plutôt que calculer des centres, l’application se charge de rechercher dans chaque cluster la chaîne de caractères qui est en moyenne la plus proche des autres. Par exemple, pour le cluster “(Alexandre, Alexia, Alexandra)”, “Alexandra” sera le pseudo-centre avec $d(Alexandre, Alexandra) = 1$, et $d(Alexandra, Alexia) = 4$ contre $d(Alexandre, Alexia) = 5$.

Enfin, la dernière différence avec l’algorithme des k-moyennes est la réassignation. Plutôt que réassigner toutes les valeurs dans les clusters, l’application ajoute une étape de vérification après l’étape de réévaluation des centres qui extrait les noms qui sont jugés comme n’étant plus pertinents, c’est-à-dire que leur distance avec le nouveau pseudo-centre est passée au-dessus de la valeur limite. La liste des noms extraite est alors celle utilisée pour l’étape de réassignation. Schématiquement, les étapes de l’algorithme ressemblent à cela :

1. Assignation des noms spécifiés dans des clusters ;
2. Réévaluation des pseudo-centres de chaque cluster ;
3. Vérification de la pertinence des clusters en extrayant une liste des noms exclus ;
4. Retour en 1. si la liste n’est pas vide.

Pour terminer, j’avais évoqué plus tôt qu’il était préférable d’utiliser une formule de calcul plutôt qu’une constante comme valeur limite pour déterminer si un nom appartient à un cluster ou non. Après plusieurs expérimentations, voici la formule que j’ai choisie :

$$
valeur(x, y) = \frac{max (longueur_x, longueur_y)}{2}
$$

Cette formule est suffisamment discriminante pour que des noms trop différents se retrouvent dans les mêmes clusters, et elle est assez tolérante pour permettre de grouper des prénoms assez éloignés en distance mais qui seraient intuitivement classés ensembles par un humain (par exemple, Alexandra et Alexia).
