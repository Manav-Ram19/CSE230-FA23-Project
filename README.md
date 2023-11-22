# CSE230-FA23-Project
A Networked application to play Battleship for CSE 230 at UCSD.

#### Team Members:
- [Manav Ram](https://github.com/Manav-Ram19)
- [Anshul Birla](https://github.com/Anshul-Birla)
- [Muhammad Zubair Khan](https://github.com/MZ-K)

## Introduction
This project aims to build a Networked Application to play the popular two player strategy game "Battleship" in Haskell!!

## Battleship
Battleship is a two-player strategy game, wherein players compete to be the first to find and destroy their opponent's ships hidden in a 2D game board. Players take turns guessing positions on the opponent's board, until one of the players has revealed all of their ships' cells, making their opponent the winner of the game.

![Battleship](/Assets/Battleship.jpeg)

(Source: [Source](https://www.ubisoft.com/en-us/game/battleship/battleship))

#### Setup
Each user starts with 5 ships of varying sizes to be placed on a two-dimensional chess board, such that no two ships lie on the same cell.

#### Game Turns
During each turn of the game, each user guesses a position on their opponent's board, and the player is informed about whether they located a cell on which one of their opponent's ships lied, or if it was an empty cell.

When a player has found all of the cells that one of their opponent's ships lies on, the ship is revealed to the player.

#### Win Condition
A player wins if all of their opponent's ships get revelead before theirs.

## Features
1. Display - The gameboards with the ship layouts being rendered on each of the player's screens.
2. Game Logic - The backend game logic that maintains game state and determines win conditions.
3. Networked Communication - The networked infrastructure that allows users on different devices to play against each other.

## Main Goals
1. Allow players to view their own board of ships during the course of the game.
2. Allow players to view revealed information of their opponents board during the course of the game.
3. Ensure real-time communication of player's moves and game states to both players across the network.
4. Display the results of a game to both players, at the end of a game.

## Reach Goals
1. Implement a timer for each timer, to safely handle disconnects, and improve the competitiveness and complexity of the game.
2. Allow players to customize the ships they pick for their board, instead of only using the 5 default ships.
3. Allow players to access and use powerups during games as they hit certain milestones during a game (eg: destroying 3 of their opponents' ships)

## Roadmap
1. Develop the low level primitive to represent a game state that will be shared across the View and Model layers of our architecture.
2. Implement the low level APIs to communicate primitives and game data over a network between the players.
3. Implement the Model layer of our architecture to house game logic, maintain game state and determine win conditions.
4. Develop and iterate on a simple but elegant UI for the application.
5. Implement reach goals as time persists.