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
   Set name for all players when you see the "Please enter your 4 player names: " prompt and the game will authomatically start.
   A list of player hands will be printed, and players will take turns moving.
4. To end the game: Control + C
