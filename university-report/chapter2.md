# La recherche de genre
## Première approche
L’outil de déduction de genre fut probablement celui qui m’a demandé le plus de réflexion préalable pour au final être le moins complexe à implémenter. En effet, comme indiqué plus haut, je souhaitais au début fournir un outil beaucoup plus poussé avec différentes règles afin d’en déduire des probabilités de genre pour un prénom donné. Ce souhait était principalement influencé par le fait qu’Étienne OLLION m’avait indiqué avoir différentes bases de noms avec de nombreuses données : pourcentage de genre pour chaque prénom, les années de naissance, etc.

J’avais alors eu comme première idée de mettre en place des règles selon ces critères en obtenant la date de naissance de l’auteur pour déterminer le genre le plus probable selon la tendance de la génération à donner le prénom à une femme ou un homme, notamment pour gérer le cas des prénoms unisexes. Cependant, après avoir demandé à Étienne OLLION des fichiers de données répondant à ces critères, j’ai réalisé que non seulement la quantité d’information pour simplement des prénoms français était très conséquente, mais aussi que les données étaient difficilement exploitables (information non homogène, parfois absente, bruitée, etc.).

C’est pourquoi nous nous sommes orientés vers une autre solution.

## Solution choisie
Nous avons alors décidé d’opter pour un simple algorithme de comparaison selon une la première base de prénoms donnée en exemple afin de permettre d’identifier simplement une partie du genre des prénoms donnés, l’avantage de cette base étant de proposer des prénoms de différentes langues.

La base de prénoms est stockée sous la forme d’un fichier *.csv*, j’ai donc dû implémenter la possibilité de parser les fichiers afin d’obtenir des données utilisables au sein de l’application. De nombreuses bibliothèques Haskell permettent la conversion et la génération de fichiers *.csv* ; j’ai opté pour **Cassava** comme me l’avait suggéré Samuel HYM. Il s’agissait de la partie présentant le plus de difficulté dans cet outil ; les fonctions associées aux parsers étant très génériques, il n’est pas évident de saisir leur utilisation sans exemple concret.

Enfin, l’identification de genre basique se résume simplement au chargement d’une base de noms, et à la recherche des noms spécifiés dans cette base pour produire une liste de prénoms genrés (ou non pour ceux absents de la base).
