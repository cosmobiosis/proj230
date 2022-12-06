# Miner

## Overview
Miner is a game we designed where user controls a miner to move up down left right to dig through the earth. The map of the game is basically a big square which is made of 21 X 21 small squares. Initially those squares are items that are covered with earth and boulders. Miner can dig up earth (brown sqaures) and creates tunnels. However, if it's a boulder (randomly distributed white square), then miner will be blocked. In the meanwhile, a monster could come and goes after him. And miner should avoid being caught by it. The miner can deploy bomb to attack monster or destroys boulders. Both would grant miner scores. Once the monster gets hit by the bomb for a certain amount of times, miner wins. If miner gets hit by the bomb or caught by the monster, the the game is lost.

## Architecture
The game architecture is built upon bricks frameworks, and key components are UI layer, Game Logic layer, and Utility Functions layer. It's sorta like MVC where the game logic layer serves as the main controller that manipulates and modifies the state. Once game state is modified, changes will be instantly reflected upon UI layer where bricks framework updates the pixels rendering based on the internal game state. For the utility functions, we have developed several mechanisms that allow us to shuffle the number array or generate random numbers or maybe some useful helpers for game rendering.

## Challenges
### Game A.I 
We plan to make Miner a PvE game so we need to implement a game AI that uses a path finding algorithm to track down the user. We first planned to use BFS algorithm however there's no mutable global deque in haskell so we change it to DFS. The path finding algorithm is really computational-intensive. So we define a max recursion levels for DFS algorithm so that the path finding won't block the main thread prgress. 

### Random number generator
random numbers are always a huge pain in the ass in haskell as haskell is a "pure" language and random numbers generation is considered as a side effect or a by product that needs to be wrapped in IO. In order to preserve the purity of most of our game logic functions, we use a pre-calculation to generate the random positions of boulders and then initialize the games with these randomly generated stats.

### Game state update
There's no OOP in haskell as it's a functional programming language! So how do we want to develop this game?! Well we manage to update every single game state using some internal variables inside game state of bricks framework. The sad part is that since there's no OOP in this game so we don't have an elegant way to pack all these variables.

## Goals
Yes we can definitely meet our goals until deadline as we finished most of the project.

## Instruction
W,A,S,D or UP,DOWN,LEFT,RIGHT for movement

SPACE for deploying the bomb

J, K for switching bomb type

R for restarting the game

Q for quitting the game

## Run Commands
stack install

stack run