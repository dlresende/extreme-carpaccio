# Instructions pour les facilitateurs

Version anglaise [ici](./README.md).

## Dépendances

- [nodejs](https://nodejs.org/en/)

## Installer et lancer

```
npm install
npm start
```

Pour lancer le serveur en mode debug (activer le mode debug pour `xcarpaccio:server`) :

```
DEBUG=xcarpaccio:server npm start
```

## Tester le réseau

Durant l'atelier, des paquets HTTP vont être échangé entre les ordinateurs des participants et celui hébergeant le serveur. Cependant nombreux sont les réseaux qui bloquent les requêtes entrantes au moyen de firewall, ce qui empêchera le serveur de joindre les ordinateurs des participants.

Avant de commencer l'atelier, il est fortement conseillé de tester le réseau que vous comptez utiliser, afin de vérifier qui accepte les requêtes entrantes. Pour cela suivez les instructions suivantes : 

1. Connectez un premier ordinateur au réseau en question
2. Sur cet ordinateur lancez: `$ echo "Hello Extreme Carpaccio" | nc -l 3000`
3. Connectez un second ordinateur au réseau
4. Sur ce second ordinateur lancez: `$ nc <IP address of the 1st computer> 3000 | tee `
5. Si le réseau accèpte les connexions entrantes, vous devriez voir apparaitre le message `Hello Extreme Carpaccio` sur le second ordinateur.

## Gelez le jeu afin de faire démarrer tout le monde en même temps

Les participants vont prendre du temps pour mettre en place leur machine, et certains mettront plus de temps que d'autres, à cause de problème de configuration.

En tant que facilitateur, vous pouvez souhaiter que tous commencent en même temps, malgré les problèmes de certains, pour le jeu soit équitable pour tous.

Vous pouvez le faire de la façon suivante : 

1. Ouvrez le ficher `configuration.json`.
2. Remplacez  `"cashFreeze": false` par  `"cashFreeze": true` puis sauvez le fichier.
3. Lancez le jeu avec `npm start`.
4. Faites que tous s'enregistre sur le serveur central.
5. Grace au paramètre ``cashFreeze`` les scores restent à 0.
6. Attendez que toutes équipes se soient enregistrées et qu'elles soient marqué **online** sur le dashboard. Le jeu est alors configuré pour tous.
7. Dites *"Il semblerait que tout le monde soit prêt, je vais lancer le jeu dans 5 secondes."*.
8. Ré-ouvrez le ficher `configuration.json`.
9. Remplacez  `"cashFreeze": true` par  `"cashFreeze": false` puis sauvez le fichier.
10. Le score des joueurs est maintenant évolutif, le jeu commence !

## L'Atelier

L'Extreme Carpaccio est pensé pour être joué avec des Product Owners (PO) et des Développeurs. Il peut être joué avec seulement des Développeurs, mais les stratégies auront alors tendance à être plus biaisées, puisque les développeurs ont plus tendance à se focaliser sur le code, plus que sur le produit ou les itérations.

L'atelier se décompose en trois étapes principales : le découpage, l'implémentation et la rétrospective. Une session normal prend entre 1:30 et 3:00 heures. 

Au début le facilitateur présente le problème à résoudre aux participants. Les participants forment alors des équipes de 2 à 4 (idéalement) personnes et essayent de comprendre et de découper le problème. Ensemble ils définissent une stratégie d'implémentation, en faisant des compromis entre la valeur produit et de défis techniques.

Ensuite le facilitateur démarre le serveur et fait en sorte que toutes les équipes soient capables d'échanger des messages HTTP avec le serveur central. Une fois que tout le monde est prêt, le facilitateur autorise les équipes à commencer à implémenter leurs solutions (peut demander un redémarrage du serveur pour remettre les scores à 0) et les gens commencent à jouer.

Durant la session, le facilitateur peut activer des "contraintes" depuis le fichier [configuration.json file](https://github.com/dlresende/extreme-carpaccio/blob/master/server/configuration.json), de façon à pimenter un peu le jeu et rajouter un peu de chaos. Quelques exemples : envoyer de mauvaises requêtes (**les participants doivent alors renvoyer une 400 - bad request**), changer la stratégie de réduction, changer le calcul des taxes, pénaliser le downtime etc. Tout changement au fichier de configuration est automatiquement pris en compte, sans nécessité de redémarrage du serveur. C'est à la discrétion du facilitateur d'annoncer ou pas les changements, dépendant de comment il souhaite mener la session.

A la fin, quand le facilitateur décide d'arrêter la partie implémentation, et que l'équipe victorieuse est déclarée, le facilitateur prend le temps d'échanger avec les participants à propos de l'exercice : ce qui a fonctionné, ce qui pourrait être amélioré, leurs feedbacks, leurs apprentissages, etc.

Je recommande fortement les gens facilitant ou jouant à Extreme Carpaccio de le tweeter utilisant le hashtag [#ExtremeCarpaccio](https://twitter.com/search?vertical=default&q=%22extreme%20carpaccio%22%20OR%20%22Xtreme%20carpaccio%22%20OR%20%23ExtremeCarpaccio&src=typd) avec leurs impressions, leur ressenti, leur feedback etc. Cela va sans dire mais sentez vous libre de forker le Repository, de faire des pull Request, de hacker le code, d'en parler, de le jouer en meetup, en conférence ou en entreprise etc.

Plus de détail à propos de l'exercice [ici](https://diegolemos.net/2016/01/07/extreme-carpaccio/) (en anglais).
