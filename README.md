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

## Architecture
Our Game consists of a client executable that users interact with to play against each other, and a server executable that facilitates communication between game clients.

![GamePlay](/Assets/GameExperience.png)

The client and server executables are built on top of the [Network.Socket](https://hackage.haskell.org/package/network-2.3/docs/Network-Socket.html) networking library, and are described below:

![GameArchitecture](/Assets/GameArchitecture.jpeg)

We define the following terms:
1. Game State - The current state of the game, involving information related to locations of both user's ships, locations of both user's attacks and whether the game is complete or if a specific player needs to play their turn.
2. Game State Update - Any changes made by one player to the game state, through making a move.

### Server Architecture
> Facilitates game state replication

Our server simply acts as a router between the two game clients of the two players playing against each other. The server receives game state updates from each client and forwards it to the other client until it has received a game state update informing it that the game is over. The server then closes the connections with both of the clients, and ends the thread handling the game.

Our Server executable is built on top of the following modules:
1. [Server Socket](ServerInfra.hs) - Builds on top of the [Network.Socket](https://hackage.haskell.org/package/network-2.3/docs/Network-Socket.html) library to provide a cleaner interface for creating a socket, as well as sending and receiving networked messages for a server use-case.
2. [Game Server](GameServer.hs) - Acts as the presenter layer of our server architecture by providing APIs to read and write messages to clients over the above server socket module.
3. [Server Game Logic](BattleShipServerLoop.hs) - Implements the actual game logic maintained by the server to decide the current player's turn and forward game updates between players. This module also determines when a game has ended, so that the presenter layer can end the connections.

### Client Architecture
> Houses the core game logic

To provide better performance, and simplicity, our client executables house the core game logic. Game Clients determine if it is its user's turn and prompt's the user to make a move, or if it is the opponent's turn and waits to receive a game state update from the opponent. By combining game state updates with their locally maintained game state, both game clients are able to rebuild the current game state to remain in coordination with each other.

Our Client executable is built on top of the following modules:
1. [Client Socket](ClientInfra.hs) - Builds on top of the [Network.Socket](https://hackage.haskell.org/package/network-2.3/docs/Network-Socket.html) library to provide a cleaner interface for creating a socket, as well as sending and receiving networked messages for a client use-case.
2. [Game Client](GameServer.hs) - Acts as the presenter layer of our client architecture by providing APIs to read and write messages to servers over the above client socket module. This layer also abstracts away the View/Display from the game logic.
3. [Client Game Logic](BattleShipClientLoop.hs) - Implements the actual game logic maintained by the client to either request the player for a move, or to wait for a game update from the opponent (received through the server).
4. Game Display (Todo) - Acts as the view for the player by utilizing the current client's local game state to display relevant information to the player. WE will be utilizing [brick](https://hackage.haskell.org/package/brick) in this layer of our architecture.

### Shared Resources
> Raw data types shared between client and server implementations

The following Raw types and methods are shared between our server and client executables:
1. [Game Types](Types.hs) - Raw data types to represent game boards, ships, cells, etc.
2. [Server Messages](ServerMessages.hs) - Provides "APIs" in the form of messages that are sent by the server, as well as encoding and decoding apis, to write and read these messages into a form supported by the lower level network sockets.
3. [Client Messages](ClientMessages.hs) - Provides "APIs" in the form of messages that are sent by the client, as well as encoding and decoding apis, to write and read these messages into a form supported by the lower level network sockets.

## Challenges

We had two main challenges until now:
1. Game Architecture - We were unsure whether the game logic should be maintained by the server, the client or both, as each approach provided its own set of advantages and disadvantages. Ultimately we ended up housing the core game logic in the client to ensure reliablility and low latency for users. Further, this also allows for scalability since our server now only acts as a simple state replication router, and can thus support more requests and games concurrently.
2. Networking Interface - The networking interface in haskell seemed a lot more complex than we had expected, and our project required us to be able to develop a reliable networking interface to prop up our game. We researched a lot about the networking libraries that we've used as well as understood the different use-cases, to finally implement this aspect of our project.

## Development Status and Future Work

We expect to be able to support all of our primary features and finish our main goals by the deadline.

## Presentation

https://www.youtube.com/watch?v=7_Y64MGK-rs

## References

- [Network-TicTacToe-Haskell](https://github.com/nikolasburk/Network-TicTacToe-Haskell): We referenced this networked tic tac toe game for two small features of only the client-service infrastructure aspect of our entire architecture: (a) Converting sockets into handles (handles made it easier to write and read data over the network) (b) managing multiple games at the same time (this was a single loc that forks our custom server logic implementation that handles game state replication between clients). The rest of our infrastructure is completely unique for the following reasons:
    - We use async network calls
    - Our server and client socket creation code is based on the example listed in the Network.Socket library's wiki: [Network.Socket](https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html)
    - We poll our handles instead of blocking on our handles, due to the specific features of our game, and how it relates to bricks (we discuss this in our presentation).
    - We have written custom APIs on top of the socket read/write methods for our client/server use cases based on battleships rules
    - Our Server implementation is responsible for the single role of state replication, but the reference game's server is responsible for input validation and game state manipulation.
    - Our server doesn't maintain any game state, as seen in our [ServerGameState implementation](https://github.com/Manav-Ram19/CSE230-FA23-Project/blob/main/src/Types.hs) (lines 42-46). On the other hand the reference game's server maintains entire game state, as seen in [Network-TicTacToe-Haskell/TTTServer.hs](https://github.com/nikolasburk/Network-TicTacToe-Haskell/blob/master/TTTServer.hs) (lines 12-17)
    - The specific lines of code that we referenced are below (the rest of the infrastructure was either built completely by us, or built using the example in the  [Network.Socket](https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html) wiki):
        - Creating Server Handle for Clients:
            - [Reference](https://github.com/nikolasburk/Network-TicTacToe-Haskell/blob/master/TTTClient.hs) (lines 48-61)
            - [Our Implementation](https://github.com/Manav-Ram19/CSE230-FA23-Project/blob/main/src/client/ClientNetwork.hs) (lines 74-78)
        - Creating Client Handles for Server:
            - [Reference](https://github.com/nikolasburk/Network-TicTacToe-Haskell/blob/master/TTTServer.hs) (lines 43-46 and 50-52)
            - [Our Implementation](https://github.com/Manav-Ram19/CSE230-FA23-Project/blob/main/src/server/GameServer.hs) (lines 130-132)
        - Server running each game on a separate thread:
            - [Reference](https://github.com/nikolasburk/Network-TicTacToe-Haskell/blob/master/TTTServer.hs) (line 57)
            - [Our Implementation](https://github.com/Manav-Ram19/CSE230-FA23-Project/blob/main/src/server/GameServer.hs) (lines 139)
- [snake](https://github.com/samtay/snake?tab=readme-ov-file) - We referenced this snake game implementation in haskell to implement a custom event with brick that gets sent out every game tick. Our use-case is unique for the following reasons:
    - [snake](https://github.com/samtay/snake?tab=readme-ov-file) uses these game ticks to move the snake every game tick. On the other hand, we use this event in our client app to poll the client's socket to check if there is any game state update from the server, as seen in our [Presenter.hs](https://github.com/Manav-Ram19/CSE230-FA23-Project/blob/main/src/client/Presenter.hs) (lines 182-190).
    - The specific lines of code that we referenced are below:
        - Creating a custom channel that sends out game events every tick:
        - [Reference](https://github.com/samtay/snake/blob/master/src/UI.hs) (lines 59-62)
        - [Our Implementation](https://github.com/Manav-Ram19/CSE230-FA23-Project/blob/main/src/client/Presenter.hs) (lines 208-213)
