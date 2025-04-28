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

   - Run the executable to start the game.

   ```
   dune exec bin/gui.exe
   ```

   - Players will be prompted to enter their names.

2. **Dealing Tiles**:

   - Each player is dealt 13 tiles at the start of the game.
   - The first player will draw an additional tile to begin their turn.

3. **Taking Turns**:

   - On your turn, you can:
     - Draw a tile from the wall.
     - Discard a tile from your hand.
   - The discarded tile becomes available for other players to claim.

4. **Claiming Discards**:

   - If a player can form a special combination (Peng=Pong=3 of the same tile, Gang=Kong = 4 of the same tile, or Chi=Chow = 3 consecutive tiles within the same group) with the discarded tile, they may claim it.
   - If multiple players want the same tile, priority is given based on the rules.

5. **Winning the Game**:

   - A player wins by forming a valid Mahjong hand (14 tiles) consisting of 4 of the above combinations as well as a double.
   - The game will automatically verify the hand's validity.

6. **Scoring**:

   [to be decided]

7. **Ending the Game**:
   - The game ends when a player wins or when the wall is exhausted.

Enjoy playing OCaMahJong!
