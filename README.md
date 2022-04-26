# TicTacJoe
This repo holds an app with TicTacJoe - a reinforcement learning example for the game of Tic Tac Toe.

Interestingly, the reinforcement learning back-end is implemented in pure R - with no libraries used.

The first version of the code for the game's engine, was developed at Itility, by Donald van den Hoogenband and Jędrzej Świeżewski.

The app is deployed to [shinyapps.io](https://swiezew.shinyapps.io/tictacjoe/).

![TicTacJoe in action!](https://github.com/Appsilon/TicTacJoe/blob/main/misc/TTJ.gif)

## Installation

Please run `renv::restore()` to install all R libraries. In case you add more libraries
please remember to update them to file `dependencies.R` and then run `renv::snapshot()`
