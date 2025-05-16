# CS3110 Final Assignment :D

## OCaMahJong

This project simulates a game of Mahjong, allowing users to play with other players
in a local multiplayer mode.

Authors:
Caedy Lee (cl2727),
Jiaxuan Wang (jw2773),
Elinor Tu (ejt74),
Phoebe Wang (pw483),
Albert Li (ajl396)

## Set Up

Please follow the directions in INSTALL.md for installation instructions.

## Gameplay

We will be loosely following the [Chinese Mahjong rules](https://www.mahjongtime.com/chinese-official-mahjong-rules.html) for our implementation. The main differences will be in point distribution (eg. no flower tiles).

1. **Starting the Game**:

   - Run `dune build`
   - Run `dune exec bin/gui.exe` to start the game.
   - The Mahjong GUI will appear:
     Set name for all players when you see the "Please enter your 4 player names: " prompt. 
     Once the names are set, press the "START" button to start the game.

2. **Dealing Tiles**:

   - Each player is dealt 13 tiles at the start of the game.
   - The first player will start by drawing an additional tile to begin their turn.

3. **Taking Turns**:

   - On your turn, you can:
     - Draw a tile from the wall.
     - Discard a tile from your hand.
     - Claim the most recently discarded tile using "Chi" or "Peng" (explained below)
   - The discarded tile becomes available for other players to claim.

4. **Claiming Discards**:

   - If a player can form a special combination (Peng=Pong=3 of the same tile, Gang=Kong = 4 of the same tile, or Chi=Chow = 3 consecutive tiles within the same group) with the discarded tile, they may claim it.
   - If multiple players want the same tile, priority is given based on the rules.

5. **Winning the Game**:

   - A player wins by forming a valid Mahjong hand (14 tiles) consisting of 4 of the above combinations as well as a double.
   - When you reach this stage, you will be provided with a "Ying" button which will end the game when clicked


6. **Ending the Game**:
   - To end the game manually, click the "close window" button in the top corner. 
   - The game ends when a player gets a winning hand or when the tiles are exhausted. 
     From the end screen, you can play a new game by clicking the "restart" button or quit the game by clicking the "quit" button. 

Enjoy playing OCaMahJong!
