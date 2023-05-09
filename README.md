# Game

Projet Bomberman en ocaml (pas très abouti, certes)

## Construction du jeu 

Il suffit de faire `dune build` à la racine. La cible construite par défaut est `prog/game_js.bc.js` qui est incluse dans le fichier HTML `index.html`. Pour construire le programme natif SDL, il faut exécuter la commande `dune build @sdl`.

