# Prérequis

- Java >= 8
- SBT >= 1.6.0

# Sujet

Il s'agit d'implémenter un pokedex en ligne de commande en utilisant l'API https://pokeapi.co/

Les commandes qui sont à implémentées sont:

- GET <pokemon-name>
- GETALL <region?> <pokemon-type1?> <generation?> <legendary?>
- WHEREIS <pokemon-name>
- MATCH <pokemon-name>

## Description des commandes :

GET <pokemon-name>

Affiche les informations du pokémon si trouvé.
Si le pokémon n'existe pas, affichez un message d'erreur.

Informations à afficher:

- nom
- type 1
- type 2
- attack
- def
- SP attack
- SP defense
- legendary
- generation
- moveset
- evolution path

GETALL <pokemon-type1?> <generation?> <legendary?>

Récupère tous les pokémons qui corresponde aux critères de recherches.
Les critères de recherche peuvent être:

- pokemon type 1 (fire, grass, water, ...)
- generation (1, 2, 3, ...)
- legendary (true or false)

- WHEREIS <pokemon-name>

Affiche les différents lieux où le pokémon peut-être vu.
Si le pokémon n'existe pas, affichez un message d'erreur.

- MATCH <keyword>

Affiche tous les noms de pokémon qui match avec le mot clé.

## Consignes:

- Utilisez au maximum l'approche fonctionnelle dans votre implémentation
- Pas de class mutable (sauf cache)
- Pas de null, utilisez plutôt Option
- Rate-limit(3 per seconds) pour éviter trop de calls API successif. (sinon risque de ban IP par pokédex API)
- Essayez d'utiliser au maximum les collections de la librairie standard
- Préférez Either pour les erreurs plutôt que les exceptions
- Séparation des fonctions pure et impure
- Gestion d' erreur en cas de paramètre invalide

## Bonus:

- Tests unitaires avec scalatest
- Utilisation de monix tasks pour encapsuler les side-effects (I/O).
- Ajout de commande supplémentaire.
- Ajout de cache
- Génération d'un roaster pokémon (6 pokémon aléatoire)
- Abstraction de pokémon API pour pouvoir tester facilement votre programme sans faire de call API. (Utilisation d'injection de dépendance)
- Proposer une nouvelle version du pokédex en API à la place d'un CLI


# Resources

https://pokeapi.co/docs/v2
https://www.scala-exercises.org/scala_tutorial/terms_and_types
https://www.scala-exercises.org/std_lib/asserts
https://docs.scala-lang.org/scala3/book/introduction.html
https://square.github.io/okhttp/
https://github.com/spray/spray-json
https://monix.io/docs/3x/eval/task.html
https://blog.appsignal.com/2022/02/16/dependency-injection-in-javascript-write-testable-code-easily.html



