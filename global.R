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

LengthOfTraining = 1000 #100000   # Total number of games until expert
InitialTemperature = 0.9
FinalTemperature = 0.03
TemperatureDecreaseStep = (FinalTemperature-InitialTemperature)/LengthOfTraining   # How much to decrease in one step of training
Temperature = InitialTemperature

# For plotting chunks
steps_in_plot_chunk = 100 #5000

# Human player
NiceColorPlayerOne = rgb(69, 177, 239, max=255) # nice shade of blue
NiceIconPlayerOne = icon("close icon")
# TicTacJoe player
NiceColorPlayerTwo = rgb(165, 81, 184, max=255)  # nice shade of purple
NiceIconPlayerTwo = icon("circle outline")

# Load precomputed game setup
PrecomputedFilesLocation = "precomputed_state"
load(file=file.path(PrecomputedFilesLocation, "States.RData"))
load(file=file.path(PrecomputedFilesLocation, "StopStates.RData"))
load(file=file.path(PrecomputedFilesLocation, "LinkedStates.RData"))
load(file=file.path(PrecomputedFilesLocation, "ProbStates.RData"))

RandomProbStates = ProbStates  # this stores the untrained TicTacJoe

# Update button after click
UpdateButton = function(WhichButton, isHuman, session) {
  # Button is from the top_left, top_middle, top_right, middle_left, ... convention
  # isHuman is TRUE if it is the user, FALSE if it is the TicTacJoe
  if(isHuman){
    shinyjs::disable(WhichButton)
    # modify to keep the color from getting greyed
    runjs(glue('document.getElementById("{WhichButton}").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = WhichButton, icon = NiceIconPlayerOne)
    } else {
    shinyjs::disable(WhichButton)
    # modify to keep the color from getting greyed
    runjs(glue('document.getElementById("{WhichButton}").style.backgroundColor = "{NiceColorPlayerTwo}";'))
    update_action_button(session, input_id = WhichButton, icon = NiceIconPlayerTwo)
  }
  return()
}

# TicTacJoe difficulty levels
TTJLevels = c("Noob", "Young Padawan", "Guru")

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
        adj_prob[index_prob_changed] = alfa[k]/10    # player 2 gets half a reward also for drawing
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