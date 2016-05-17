# Identification et classification des noms
## La recherche de genre
### Première approche
L’outil de déduction de genre fut probablement celui qui m’a demandé le plus de réflexion préalable pour au final être le moins complexe à implémenter. En effet, comme indiqué plus haut, je souhaitais au début fournir un outil beaucoup plus poussé avec différentes règles afin d’en déduire des probabilités de genre pour un prénom donné. Ce souhait était principalement influencé par le fait qu’Étienne OLLION m’avait indiqué avoir différentes bases de noms avec de nombreuses données : pourcentage de genre pour chaque prénom, les années de naissance, etc.

J’avais alors eu comme première idée de mettre en place des règles selon ces critères en obtenant la date de naissance de l’auteur pour déterminer le genre le plus probable selon la tendance de la génération à donner le prénom à une femme ou un homme, notamment pour gérer le cas des prénoms unisexes. Cependant, après avoir demandé à Étienne OLLION des fichiers de données répondant à ces critères, j’ai réalisé que non seulement la quantité d’information pour simplement des prénoms français étaient très conséquente, mais aussi que les données étaient difficilement exploitables (information non homogène, parfois absente, bruitée, etc.).

C’est pourquoi nous nous sommes orientés vers une autre solution.

### Solution choisie
Nous avons alors décidé d’opter pour un simple algorithme de comparaison selon une la première base de prénoms donnée en exemple afin de permettre d’identifier simplement une partie du genre des prénoms donnés, l’avantage de cette base étant de proposer des prénoms de différentes langues.

La base de prénoms est stockée sous la forme d’un fichier *.csv*, j’ai donc dû implémenter la possibilité de parser les fichiers afin d’obtenir des données utilisables au sein de l’application. De nombreuses bibliothèques Haskell permettent la conversion et la génération de fichiers *.csv* ; j’ai opté pour **Cassava** comme me l’avait suggéré Samuel HYM. Il s’agissait de la partie présentant le plus de difficulté dans cet outil ; les fonctions associées aux parsers étant très génériques, il n’est pas évident de saisir leur utilisation sans exemple concret.

[TODO: expliquer l’algorithme]

## Regroupement de noms
L’outil de regroupement de noms fut le premier outil que j’ai implémenté au cours du projet. L’objectif principal était d’avoir un moyen de présenter les prénoms d’une autre façon que via une simple liste, pouvant donner lieu à des groupes pertinents (Christine, Kirsten, Kristen, prénoms tous féminins)… ou non (Alexandre, Alexandra, prénoms de genre différent).

### Notion de distance
Pour aborder cet algorithme, j’ai commencé par une implémentation basique se contentant de placer les noms donnés dans des groupes appelés “clusters” selon certains critères. Concrètement, voici ce que faisait ma première implémentation d’algorithme : pour chaque nom *A* à classer, on recherche un cluster dont le premier nom a une distance du nom *A* inférieure à une constante prédéfinie. Si aucun cluster n’a été trouvé, le prénom *A* forme un nouveau cluster.

J’évoque ci-dessus la notion de distance. De quoi s’agit-il exactement ? J’ai utilisé ici la **distance de Levenshtein**. Il s’agit d’une notion mathématique que nous utilisons dans notre cas pour évaluer le degré de similitude de deux chaînes de caractères. Concrètement, il s’agit d’une nombre d’opérations de caractères (substitution, ajout, suppression) pour transformer une chaîne en l’autre. Ainsi, pour passer du prénom *Alexandre* à *Alexandra*, il est nécessaire de subtituer la dernière lettre (*a* devient *e*, ou *e* devient *a*) ; la distance de Levenshtein vaut alors 1.

Cette implémentation s’avéra être une base convenable pour la suite, mais elle présentait bien évidemment plusieurs défauts. Tout d’abord, l’utilisation du premier nom du cluster n’est absolument pas représentatif du cluster. Ensuite, l’utilisation d’une constante prédéfinie résultera en des résultats hétérogènes pour des noms de longueur très variée. Il était alors plus judicieux d’utiliser une variable dépendant de la longueur des mots plutôt qu’une constante.

### Partitionnement en k-moyennes
La version finale de notre algorithme de classification est inspirée de celui de partitionnement en k-moyennes (ou *k-means* en anglais), que nous allons expliquer ici afin de mieux comprendre ce qui m’a mené à l’algorithme obtenu. Cet algorithme est utilisé pour classifier des données en un nombre prédéfinis de groupes selon une distance définie. Il est généralement nécessaire de transformer les données afin d’obtenir un problème purement géométrique (c’est-à-dire, que des données sous forme de chiffres). Les clusters possèdent tous un centre qui représente la moyenne des données, et l’algorithme consiste en deux étapes : une étape d’**assignation**, et une étape de **réévaluation**.

Durant l’étape d’assignation, chaque donnée est placée dans le cluster dont elle est le plus proche ; on calcule pour cela la distance de la donnée au centre du cluster. Une fois toutes les données placées (et donc l’étape d’assignation terminée), on recalcule tous les centres des clusters selon la moyenne des données contenues dans chaque : il s’agit de l’étape de réévaluation. Enfin, dans le cas où des données ont changé de groupes durant l’étape d’assignation, on réassigne à nouveau toutes les données aux clusters, et on réévalue les centres, et ainsi de suite… jusqu’à qu’aucune donnée ne change de groupe. En résumé :

1. On définit des centres initiaux pour tous les clusters.
2. On assigne toutes les données à un cluster.
3. On réévalue les centres des clusters.
4. Retour en 2. si des valeurs ont changé de groupes.

### Version finale de l’algorithme

