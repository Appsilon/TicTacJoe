library(shiny)
library(shiny.semantic)
library(shinyjs)
library(glue)

# Initialize needed parameters/variables
N = 9 # Board size
n = sqrt(N)
p = 3
alfa_one = 0.5 # Relative convergence of probabilities of top level
alfa_two = 0.6 # Relative convergence of probabilities of bottom level
alfa = seq(alfa_one,alfa_two,(alfa_two-alfa_one)/N)

LengthOfTraining = 10000 #1000 #100000  # Total number of games until expert
InitialTemperature = 0.9
FinalTemperature = 0.03
TemperatureDecreaseStep = (FinalTemperature-InitialTemperature)/LengthOfTraining   # How much to decrease in one step of training
Temperature = InitialTemperature

# For plotting chunks
steps_in_plot_chunk = 200 #100 #500

# User
NiceColorUser = rgb(69, 177, 239, max=255) # nice shade of blue
NiceIconUser <- HTML("<i class='close icon icon'></i>")

# TTJ
NiceColorTTJ = rgb(165, 81, 184, max=255)  # nice shade of purple
NiceIconTTJ = icon("circle outline")

# Load precomputed game setup
PrecomputedFilesLocation = "precomputed_state"
load(file=file.path(PrecomputedFilesLocation, "States.RData"))
load(file=file.path(PrecomputedFilesLocation, "StopStates.RData"))
load(file=file.path(PrecomputedFilesLocation, "LinkedStates.RData"))
load(file=file.path(PrecomputedFilesLocation, "ProbStates.RData"))

RandomProbStates = ProbStates  # this stores the untrained TTJ

# Update button after click
UpdateButton = function(WhichButton, toState, session) {
  # Button is from the top_left, top_middle, top_right, middle_left, ... convention
  # toState is "user" if the user clicked, "TTJ" if it is TTJs move, "default" if button should be returned to unpressed state
  if(toState == "user") {
    shinyjs::disable(WhichButton)
    # modify to keep the color from getting greyed
    runjs(glue('document.getElementById("{WhichButton}").style.backgroundColor = "{NiceColorUser}";'))
    runjs(glue('document.getElementById("{WhichButton}").innerHTML = "{NiceIconUser}";'))
  } else if(toState == "TTJ") {
    shinyjs::disable(WhichButton)
    # modify to keep the color from getting greyed
    runjs(glue('document.getElementById("{WhichButton}").style.backgroundColor = "{NiceColorTTJ}";'))
    update_action_button(session, input_id = WhichButton, icon = NiceIconTTJ)
  } else if(toState == "default") {
    shinyjs::enable(WhichButton)
    runjs(glue('document.getElementById("{WhichButton}").style.backgroundColor = "{rgb(225, 225, 225, max=255)}";'))
    runjs(glue('document.getElementById("{WhichButton}").innerHTML = "";'))
    # update_action_button(session, input_id = WhichButton, icon = NULL, label = "")
  }
  return()
}

# TicTacJoe difficulty levels
TTJLevels = c("Noob", "Young Padawan", "Guru")

# Board tile names
BoardTileNames = c("top_left", "top_middle", "top_right",
                   "middle_left", "middle_middle", "middle_right",
                   "bottom_left", "bottom_middle", "bottom_right"
                   )

# Define Normalization function
Normalize = function(Vector) {
  return(Vector/sum(Vector))
}

# Define softmax function
Softmax = function(Vector, Temperature) {
  ResultVector = Normalize(sapply(Vector/Temperature,exp))
  return(ResultVector)
}

# Create a function which runs one path and updates the Markov chain model probabilities
UpdateProbabilitiesUsingPath = function(path, StopStates, LinkedStates, ProbStates, updateplayer, Temperature) {
  L = length(path)
  path_result = StopStates[[L]][[path[L]]]
  for (k in 2:L) {
    if (updateplayer==0) {
      player = (k %% 2) + 1
    } else {
      player = updateplayer
    }
    calculate_probabilities = ProbStates[[k-1]][[path[k-1]]]
    index_prob_changed = which(LinkedStates[[k-1]][[path[k-1]]]==path[[k]])
    adj_prob = numeric(length(calculate_probabilities))
    if (player==1 && ((k %% 2) + 1)==1) {
      if (path_result==1) {
        adj_prob[index_prob_changed] = alfa[k]
      } else if(path_result==0) {
        adj_prob[index_prob_changed] = 0
      } else if(path_result==2) {
        adj_prob[index_prob_changed] = -alfa[k]
      }
    } else if (player==2 && ((k %% 2) + 1)==2) {
      if (path_result==2) {
        adj_prob[index_prob_changed] = alfa[k]
      } else if(path_result==0) {
        adj_prob[index_prob_changed] = alfa[k]/10    # second player gets partial reward also for drawing
      } else if(path_result==1) {
        adj_prob[index_prob_changed] = -alfa[k]
      }
    }

    ProbStates[[k-1]][[path[k-1]]] = Softmax(calculate_probabilities + adj_prob, Temperature)
  }

  return(ProbStates)
}

# Run a game of TicTacJoe vs TicTacJoe
RunTicTacToeComputerVSComputer = function(States,StopStates,LinkedStates,ProbStates,Temperature) {
  N = length(ProbStates)-1
  Path_Run = 1
  Selected_Moves = runif(N,0,1)
  Probabilities = cumsum(ProbStates[[1]][[1]])
  i=2
  while (StopStates[[i-1]][[Path_Run[i-1]]]==0 && i<=9) {
    Probabilities = cumsum(ProbStates[[i-1]][[Path_Run[i-1]]])
    Path_Run[i] = LinkedStates[[i-1]][[Path_Run[i-1]]][which(Selected_Moves[[i-1]] < Probabilities)[1]]
    i = i + 1
  }
  if (length(Path_Run)==N) {
    if (StopStates[[length(Path_Run)]][[Path_Run[length(Path_Run)]]] == 0) {
      Path_Run[N+1] = LinkedStates[[N]][[Path_Run[length(Path_Run)]]]
    }
  }
  ProbStates = UpdateProbabilitiesUsingPath(Path_Run,StopStates,LinkedStates,ProbStates,0,Temperature)
  return(ProbStates)
}

# Find equivalent states of the board - from a current_boardstate, e.g., 2 1 0 0 1 2 2 1 1, produce a collection of equivalent states.
# A state is equivalent if it can be reached by a rotation or flip.
# There are always 8 equivalent states, some may be identical
GetEquivalentStates = function(current_boardstate) {
  equivalent_states = matrix(0,nrow=8,ncol=N)
  equivalent_states[1,1:N] = current_boardstate
  equivalent_states[2,1:N] = c(apply(t(matrix(equivalent_states[1,1:N],nrow=n,ncol=n)), 2, rev))
  equivalent_states[3,1:N] = c(apply(t(matrix(equivalent_states[2,1:N],nrow=n,ncol=n)), 2, rev))
  equivalent_states[4,1:N] = c(apply(t(matrix(equivalent_states[3,1:N],nrow=n,ncol=n)), 2, rev))
  equivalent_states[5,1:N] = c(apply(matrix(equivalent_states[1,1:N],nrow=n,ncol=n), 2, rev))
  equivalent_states[6,1:N] = c(apply(matrix(equivalent_states[2,1:N],nrow=n,ncol=n), 2, rev))
  equivalent_states[7,1:N] = c(apply(matrix(equivalent_states[3,1:N],nrow=n,ncol=n), 2, rev))
  equivalent_states[8,1:N] = c(apply(matrix(equivalent_states[4,1:N],nrow=n,ncol=n), 2, rev))
  return(equivalent_states[do.call(order, as.data.frame(equivalent_states)),])
}

# Check if someone won the game (returns: "user", "TTJ", "draw", or NULL in case no winner yet)
CheckIfWon = function(StopStates, PathRun, move_nr, user_code) {
  stop_state = StopStates[[move_nr+1]][[PathRun[move_nr+1]]]
  if(stop_state == 1) {
    # First player won
    if(user_code == 1) {
      winner = "user"
    } else {
      winner = "TTJ"
    }
  } else if(stop_state == 2) {
    # Second player won
    if(user_code == 1) {
      winner = "TTJ"
    } else {
      winner = "user"
    }
  } else if(move_nr == 9) {
    # It's a draw
    winner = "draw"
  } else {
    winner = NULL
  }
  return(winner)
}

# Display who won
DisplayWinner = function(winner) {
  print(paste("The winner is:", winner))
  if(winner == "TTJ") {
    text_to_show = "You lost to TicTacJoe!"
    addClass(id = "game_modal_header", class="game_ended_lost")
  } else if (winner == "user") {
    text_to_show = "You won!"
    addClass(id = "game_modal_header", class="game_ended_won")
  } else if (winner == "draw") {
    text_to_show = "It's a draw"
  }
  runjs(glue('document.getElementById("game_header").textContent = "{text_to_show}";'))
  addClass(id = "game_modal_header", class="game_ended")
}

# Hide who won the last game (e.g., when starting a new one)
HideWinner = function() {
  runjs(glue('document.getElementById("game_header").textContent = "Let\'s play a game!";'))
  removeClass(id = "game_modal_header", class="game_ended")
  removeClass(id = "game_modal_header", class="game_ended_won")
  removeClass(id = "game_modal_header", class="game_ended_lost")
}
