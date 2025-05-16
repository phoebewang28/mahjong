## Installation

1. Download/clone this project onto your computer

```
git clone <repository-url>
```

2. Open the terminal in the root folder

```
cd <repository-name>
```

3. The following ocaml libraries need to be installed: raylib, raygui, ounit2.

```
opam update
opam upgrade
opam install raylib raygui ounit2 containers lwt_ppx ANSITerminal bisect_ppx qcheck yojson re
```

## Running the Game

1. Run `dune build`
2. Run `dune exec bin/gui.exe` to start the game.
3. The Mahjong GUI will appear:
   Set name for all players when you see the "Please enter your 4 player names: " prompt. 
   Once the names are set, press the "START" button to start the game.
4. To end the game manually, click the "close window" button in the top corner. 
5. The game ends when a player gets a winning hand or when the tiles are exhausted. 
   From the end screen, you can play a new game by clicking the "restart" button or quit the game by clicking the "quit" button. 