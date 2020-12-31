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

LengthOfTraining = 100000   # Total number of games until expert
InitialTemperature = 0.9
FinalTemperature = 0.03
TemperatureDecreaseStep = (FinalTemperature-InitialTemperature)/LengthOfTraining   # How much to decrease in one step of training

NiceColorPlayerOne = rgb(232, 85, 85, max=255)  # nice shade of red
NiceColorPlayerTwo = rgb(69, 177, 239, max=255) # nice shade of blue

# Load precomputed game setup
PrecomputedFilesLocation = "precomputed_state"
load(file=file.path(PrecomputedFilesLocation, "States.RData"))
load(file=file.path(PrecomputedFilesLocation, "StopStates.RData"))
load(file=file.path(PrecomputedFilesLocation, "LinkedStates.RData"))
load(file=file.path(PrecomputedFilesLocation, "ProbStates.RData"))

RandomProbStates = ProbStates  # this stores the untrained TicTacJoe


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
