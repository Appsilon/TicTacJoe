# TicTacJoe
This repo holds a demo app with TicTacJoe - a reinforcement learning example for the game of Tic Tac Toe.

Interestingly, the reinforcement learning back-end is implemented in pure R - with no libraries used.

The app is deployed to the [internal connect server](https://internal.appsilon.ai/content/217/).

The first version of the code for the game (with some very basic UI - can be found in the x11_version folder), was developed at Itility, by Donald van den Hoogenband and Jędrzej Świeżewski.

# TODO
- Polish the graphs not to be grayed out when updating during training
- Optimize code to speed up training
- Rearrange UI nicely

- **DONE** "End game" button should close modal with game
- **DONE** Let user choose who starts (now it is random)
- **DONE** Fix UpdateButton so that it uses the update_action_button as well
- **DONE** Make training update state of TTJ
- **DONE** Add flushing training,
- **DONE** Enable/disable training and flushing
- **DONE** Prevent the disabled moves from greying out (modify UpdateButton)
- **DONE** Run game - alternating and storing progress
- **DONE** Display saturation for a given move (?)
- **DONE** Cancel game also when user clicks next to the modal
- **DONE** Return tile color to default not black when game is cancelled
- **DONE** Display who won nicely
- **DONE** Fix: display icon immediately after users move (not after TTJ moves)
- **DONE** Fix: display probabilities of a move on tiles (now they are shown and hidden immediately)
- **DONE** prevent the "first" button to be "hovered-on" when loading a page (main and the game as well)
- **DONE** Fix: after TTJ wins, in new game that he starts he doesn't make the move
