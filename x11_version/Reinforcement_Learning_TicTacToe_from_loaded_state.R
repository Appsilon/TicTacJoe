## The following code defines a game of Tic-Tac-Toe ##
# The AI is personified as TicTacJoe and it is trained using reinforcement learning.
# Execute the entire script to start the game.

# Initialize needed parameters/variables
N=9
n = sqrt(N)
p = 3
alfa_one = 0.5 # Relative convergence of probabilities of top level
alfa_two = 0.6 # Relative convergence of probabilities of bottom level
alfa = seq(alfa_one,alfa_two,(alfa_two-alfa_one)/N)

LengthOfTraining = 100000   # Total length until expert
InitialTemperature = 0.9
FinalTemperature = 0.03
TemperatureDecreaseStep = (FinalTemperature-InitialTemperature)/LengthOfTraining   # How much to decrease in one step of training

NiceColorPlayerOne = rgb(232, 85, 85, max=255)  # nice shade of red
NiceColorPlayerTwo = rgb(69, 177, 239, max=255) # nice shade of blue


# Load precomputed game setup
PrecomputedFilesLocation = "../precomputed_state"
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

# Plot a state of TicTacToe
PlotTicTacToe = function(BoardState,CurrentStopState,HumanPlayer=NULL) {   # the optional HumanPlayer allows the function to know which player was Human when plotting who won
  # example BoardState = c(2,1,2,1,0,1,2,1,2)
  
  # Create the TicTacToe Field
  plot(c(0.00,0.46),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
  lines(c(0.00,0.46),c(0,0),cex=1,lwd=1,type="l")
  lines(c(0.00,0.00),c(0,1),cex=1,lwd=1,type="l")
  lines(c(0.46,0.46),c(0,1),cex=1,lwd=1,type="l")
  lines(c(0.00,0.46),c(0.33,0.33),cex=1,lwd=1,type="l")
  lines(c(0.00,0.46),c(0.66,0.66),cex=1,lwd=1,type="l")
  lines(c(0.15,0.15),c(0,1),cex=1,lwd=1,type="l")
  lines(c(0.30,0.30),c(0,1),cex=1,lwd=1,type="l")
  
  color_map = c(`0`="white",`1`=NiceColorPlayerOne,`2`=NiceColorPlayerTwo)
  color_vector = (color_map[as.character(BoardState)])
  shape_map = c(`0`=1,`1`=4,`2`=1)
  shape_vector = as.numeric(shape_map[as.character(BoardState)])
    
  points(0.08,0.84,cex=8,lwd=5,pch=shape_vector[1],col=color_vector[1])
  points(0.23,0.84,cex=8,lwd=5,pch=shape_vector[2],col=color_vector[2])
  points(0.38,0.84,cex=8,lwd=5,pch=shape_vector[3],col=color_vector[3])
  points(0.08,0.50,cex=8,lwd=5,pch=shape_vector[4],col=color_vector[4])
  points(0.23,0.50,cex=8,lwd=5,pch=shape_vector[5],col=color_vector[5])
  points(0.38,0.50,cex=8,lwd=5,pch=shape_vector[6],col=color_vector[6])
  points(0.08,0.16,cex=8,lwd=5,pch=shape_vector[7],col=color_vector[7])
  points(0.23,0.16,cex=8,lwd=5,pch=shape_vector[8],col=color_vector[8])
  points(0.38,0.16,cex=8,lwd=5,pch=shape_vector[9],col=color_vector[9])
  
  if(CurrentStopState == 1) {
    if (HumanPlayer == 1) {
      text(0.75,0.8,labels = c("You won"), font=1, col=1, cex=3)
    } else {
      text(0.75,0.8,labels = c("TicTacJoe won"), font=1, col=1, cex=3)
    }
    points(0.75,0.3,cex=12,lwd=5,pch=4,col=NiceColorPlayerOne)
    # plot the crown:
    points(0.70,0.5,cex=9,lwd=5,pch=17,col="gold")
    points(0.75,0.5,cex=9,lwd=5,pch=17,col="gold")
    points(0.80,0.5,cex=9,lwd=5,pch=17,col="gold")
    points(0.65,0.5,cex=14,lwd=5,pch=15,col="white")
    points(0.85,0.5,cex=14,lwd=5,pch=15,col="white")
  } else if(CurrentStopState == 2) {
    if (HumanPlayer == 2) {
      text(0.75,0.8,labels = c("You won"), font=1, col=1, cex=3)
    } else {
      text(0.75,0.8,labels = c("TicTacJoe won"), font=1, col=1, cex=3)
    }
    points(0.75,0.3,cex=12,lwd=5,pch=1,col=NiceColorPlayerTwo)
    # plot the crown:
    points(0.70,0.5,cex=9,lwd=5,pch=17,col="gold")
    points(0.75,0.5,cex=9,lwd=5,pch=17,col="gold")
    points(0.80,0.5,cex=9,lwd=5,pch=17,col="gold")
    points(0.65,0.5,cex=14,lwd=5,pch=15,col="white")
    points(0.85,0.5,cex=14,lwd=5,pch=15,col="white")
  }
  if(CurrentStopState == 0) {
    if (sum(BoardState) == 13) {
      text(0.75,0.8,labels = c("It is a draw"), font=1, col=1, cex=2)
    } else {
      if (sum(BoardState) %% 3 == 0) {
        text(0.75,0.85,labels = c("Next move:      "), font=1, col=1, cex=2)
        points(0.86,0.85,cex=8,lwd=5,pch=4,col=NiceColorPlayerOne)
      } else {
        text(0.75,0.85,labels = c("Next move:      "), font=1, col=1, cex=2)
        points(0.86,0.85,cex=8,lwd=5,pch=1,col=NiceColorPlayerTwo)
      }
    }
  }
}

# Run a game of TicTacToe Player vs Computer
RunTicTacToePlayerVSComputer = function(States,StopStates,LinkedStates,ProbStates,Temperature) {
  PlotTicTacToe(rep(0,N),0)
  #menu(c("Top - Left","Top - Middle","Top - Right",
  #       "Middle - Left","Middle - Middle","Middle - Right",
  #       "Bottom - Left","Bottom - Middle","Bottom - Right"),
  #     graphics = TRUE, title="Choose your next move")
  WhoStarts = menu(c("Randomly chosen","You","TicTacJoe"),
                     graphics = TRUE, title="Who begins?")
  if(WhoStarts==1) {
    HumanPlayer = sample(2,1)
    ComputerPlayer = 3 - HumanPlayer
  }
  if (WhoStarts==2) {
    HumanPlayer = 1
    ComputerPlayer = 2
  }
  if (WhoStarts==3) {
    HumanPlayer = 2
    ComputerPlayer = 1
  }
  
  # Start the game
  N = length(ProbStates)-1
  Path_Run = 1
  Selected_Moves = runif(N,0,1)
  Probabilities = cumsum(ProbStates[[1]][[1]])
  i=2
  current_boardstate = rep(0,N)
  unique_current_boardstate = current_boardstate
  while (StopStates[[i-1]][[Path_Run[i-1]]]==0 && i<=9) {
    if (((i %% 2) + 1) == HumanPlayer) {
      PlotTicTacToe(current_boardstate,StopStates[[i-1]][[Path_Run[i-1]]])
      All_Moves = 1:N
      Possible_Moves = All_Moves[current_boardstate == 0]
      Possible_Text = character(0)
      for (j in 1:length(Possible_Moves)) {
        if (Possible_Moves[j]==1) {
          Possible_Text = c(Possible_Text,"Top - Left")
        } else if(Possible_Moves[j]==2) {
          Possible_Text = c(Possible_Text,"Top - Middle")
        } else if(Possible_Moves[j]==3) {
          Possible_Text = c(Possible_Text,"Top - Right")
        } else if(Possible_Moves[j]==4) {
          Possible_Text = c(Possible_Text,"Middle - Left")
        } else if(Possible_Moves[j]==5) {
          Possible_Text = c(Possible_Text,"Middle - Middle")
        } else if(Possible_Moves[j]==6) {
          Possible_Text = c(Possible_Text,"Middle - Right")
        } else if(Possible_Moves[j]==7) {
          Possible_Text = c(Possible_Text,"Bottom - Left")
        } else if(Possible_Moves[j]==8) {
          Possible_Text = c(Possible_Text,"Bottom - Middle")
        } else if(Possible_Moves[j]==9) {
          Possible_Text = c(Possible_Text,"Bottom - Right")
        }
      }
      player_move = menu(Possible_Text, graphics = TRUE, title="Choose your next move")
      player_move = Possible_Moves[player_move]
      current_boardstate[player_move] = HumanPlayer
      similar_next_states = matrix(0,nrow=8,ncol=N) # Number of similarly rotated and/or Mirrored state
      similar_next_states[1,1:N] = current_boardstate
      similar_next_states[2,1:N] = c(apply(t(matrix(similar_next_states[1,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[3,1:N] = c(apply(t(matrix(similar_next_states[2,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[4,1:N] = c(apply(t(matrix(similar_next_states[3,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[5,1:N] = c(apply(matrix(similar_next_states[1,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[6,1:N] = c(apply(matrix(similar_next_states[2,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[7,1:N] = c(apply(matrix(similar_next_states[3,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[8,1:N] = c(apply(matrix(similar_next_states[4,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states = similar_next_states[do.call(order, as.data.frame(similar_next_states)),]
      unique_current_boardstate = c(similar_next_states[8,])
      for (j in 1:length(LinkedStates[[i-1]][[Path_Run[i-1]]])) {
        if(all(States[[i]][[LinkedStates[[i-1]][[Path_Run[i-1]]][j]]]==unique_current_boardstate)) {
          Path_Run[i] = LinkedStates[[i-1]][[Path_Run[i-1]]][j]
          break
        }
      }
      i = i + 1
    } else {
      PlotTicTacToe(current_boardstate,StopStates[[i-1]][[Path_Run[i-1]]])
      text(0.75,0.1,labels = paste("level of confidence:", round(max(ProbStates[[i-1]][[Path_Run[i-1]]])*100, 0),"%"), font=1, col=1, cex=2)
      text(0.75,0,labels = paste("all probabilities are:", paste(round(ProbStates[[i-1]][[Path_Run[i-1]]], 3),collapse=", ")), font=1, col=1, cex=1)
      Sys.sleep(4)
      Probabilities = cumsum(ProbStates[[i-1]][[Path_Run[i-1]]])
      Path_Run[i] = LinkedStates[[i-1]][[Path_Run[i-1]]][which(Selected_Moves[[i-1]] < Probabilities)[1]]
      unique_current_boardstate = States[[i]][[Path_Run[[i]]]]
      similar_next_states = matrix(0,nrow=8,ncol=N) # Number of similarly rotated and/or Mirrored state
      similar_next_states[1,1:N] = unique_current_boardstate
      similar_next_states[2,1:N] = c(apply(t(matrix(similar_next_states[1,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[3,1:N] = c(apply(t(matrix(similar_next_states[2,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[4,1:N] = c(apply(t(matrix(similar_next_states[3,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[5,1:N] = c(apply(matrix(similar_next_states[1,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[6,1:N] = c(apply(matrix(similar_next_states[2,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[7,1:N] = c(apply(matrix(similar_next_states[3,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[8,1:N] = c(apply(matrix(similar_next_states[4,1:N],nrow=n,ncol=n), 2, rev))
      current_boardstate = similar_next_states[which(rowSums(similar_next_states == t(matrix(rep(current_boardstate,8),nrow=N,ncol=8)))==8)[1],]
      i = i + 1
    }
  }
  if (length(Path_Run)==N) {
    if (StopStates[[length(Path_Run)]][[Path_Run[length(Path_Run)]]] == 0) {
      PlotTicTacToe(current_boardstate,StopStates[[i-1]][[Path_Run[i-1]]])
      Sys.sleep(4)
      Path_Run[N+1] = LinkedStates[[N]][[Path_Run[length(Path_Run)]]]
      unique_current_boardstate = States[[i]][[Path_Run[[i]]]]
      similar_next_states = matrix(0,nrow=8,ncol=N) # Number of similarly rotated and/or Mirrored state
      similar_next_states[1,1:N] = unique_current_boardstate
      similar_next_states[2,1:N] = c(apply(t(matrix(similar_next_states[1,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[3,1:N] = c(apply(t(matrix(similar_next_states[2,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[4,1:N] = c(apply(t(matrix(similar_next_states[3,1:N],nrow=n,ncol=n)), 2, rev))
      similar_next_states[5,1:N] = c(apply(matrix(similar_next_states[1,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[6,1:N] = c(apply(matrix(similar_next_states[2,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[7,1:N] = c(apply(matrix(similar_next_states[3,1:N],nrow=n,ncol=n), 2, rev))
      similar_next_states[8,1:N] = c(apply(matrix(similar_next_states[4,1:N],nrow=n,ncol=n), 2, rev))
      current_boardstate = similar_next_states[which(rowSums(similar_next_states == t(matrix(rep(current_boardstate,8),nrow=N,ncol=8)))==8)[1],]
      i = i + 1
    }
  }
  PlotTicTacToe(current_boardstate,StopStates[[i-1]][[Path_Run[i-1]]],HumanPlayer = HumanPlayer)
  Sys.sleep(5)
  ProbStates = UpdateProbabilitiesUsingPath(Path_Run,StopStates,LinkedStates,ProbStates,ComputerPlayer,Temperature)
  return(ProbStates)
}


# Run a game of TicTacToe Computer vs Computer
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

####### Game interface

# display face depending on GameLevel
DisplayFace = function(GameLevel) {  # GameLevel = 1 -> Noob, 2 -> Medium, 3 -> Godmode
  if(GameLevel == 1){
    YPositionOfBox = 0.4;
    TextToShow = c("TicTacJoe is currently a noob");
  } 
  if(GameLevel == 2) {
    YPositionOfBox = 0.43;
    TextToShow = c("TicTacJoe is a young padawan");
  }
  if(GameLevel == 3) {
    YPositionOfBox = 0.47;
    TextToShow = c("TicTacJoe is a TicTacToe expert");
  }
  # plot title
  plot(c(0.00,0.45),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
  lines(c(0.55,1   ),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
  text(0.5,1,labels = c("TicTacJoe"), font=1, col=1, cex=1)
  # plot face
  points(0.25 ,0.37,cex=16,lwd=5,pch=1,col="black")
  points(0.25 ,YPositionOfBox ,cex=20,lwd=5,pch=15,col="white")
  points(0.25 ,0.4 ,cex=24,lwd=5,pch=1,col="black")
  points(0.22,0.43 ,cex=7,lwd=5,pch=4,col=NiceColorPlayerOne)
  points(0.29,0.43 ,cex=7,lwd=5,pch=1,col=NiceColorPlayerTwo)
  # plot level of TicTacJoe
  text(0.25,0.7,labels = TextToShow, font=1, col=1, cex=1)
}

# Start the adventure mode!
StartAdventure = function(){
  GameIsOn = TRUE
  x11(width = 145,height = 100,xpos=0,ypos=50)
  # plot title
  plot(c(0.00,0.45),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
  lines(c(0.55,1   ),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
  text(0.5,1,labels = c("TicTacJoe"), font=1, col=1, cex=1)
  # plot face
  points(0.5 ,0.37,cex=16,lwd=5,pch=1,col="black")
  points(0.5 ,0.47 ,cex=20,lwd=5,pch=15,col="white")
  points(0.5 ,0.4 ,cex=24,lwd=5,pch=1,col="black")
  points(0.47,0.43 ,cex=7,lwd=5,pch=4,col=NiceColorPlayerOne)
  points(0.54,0.43 ,cex=7,lwd=5,pch=1,col=NiceColorPlayerTwo)
  # plot welcome message
  text(0.5,0.7,labels = c("Welcome to TicTacJoe!"), font=1, col=1, cex=2)
  Sys.sleep(3)
  # initialize Temperature
  Temperature = InitialTemperature
  # initialize GameLevel
  GameLevel = 1
  # flush ProbStates
  ProbStates=RandomProbStates
  
  while(GameIsOn == TRUE){
    DisplayFace(GameLevel)
    if(GameLevel < 3){
      WhatToDo = menu(c("Play against TicTacJoe","Let TicTacJoe train a bit","Flush TicTacJoe's training","Leave the game"), graphics = TRUE, title="What would you like to do?")
    } else {
      WhatToDo = menu(c("Play against TicTacJoe","Flush TicTacJoe's training","Leave the game"), graphics = TRUE, title="What would you like to do?")
    }
    if (WhatToDo == 1){
      # Play against TicTacJoe
      ProbStates = RunTicTacToePlayerVSComputer(States,StopStates,LinkedStates,ProbStates,Temperature)
      next
    } else if (GameLevel < 3 && WhatToDo == 2){
      # Let TicTacJoe train to get to the next level
      StepOne = floor(LengthOfTraining/6)
      StepTwo = floor(LengthOfTraining/3)
      text(0.215,0.15,labels = c("training in progress"), font=1, col=1, cex=1)
      for (i in 0:floor(LengthOfTraining/2)) {
        ProbStates = RunTicTacToeComputerVSComputer(States,StopStates,LinkedStates,ProbStates,Temperature)
        Temperature = Temperature + TemperatureDecreaseStep
        if(i == StepOne){
          text(0.3,0.142,labels = c("..."), font=1, col=1, cex=1)
        }
        if(i == StepTwo) {
          text(0.315,0.142,labels = c("..."), font=1, col=1, cex=1)
        }
        print(i)
      }
      GameLevel = GameLevel + 1
      text(0.347,0.152,labels = c("done!"), font=1, col=1, cex=1)
      Sys.sleep(2)
      next
    } else if ((GameLevel < 3 && WhatToDo == 3) || (GameLevel == 3 && WhatToDo == 2)){
      # Flush Temperature
      Temperature = InitialTemperature
      # Flush TicTacJoe
      ProbStates = RandomProbStates
      # Flush GameLevel
      GameLevel = 1
      next
    } else if ((GameLevel < 3 && WhatToDo == 4) || (GameLevel == 3 && WhatToDo == 3)){
      GameIsOn = FALSE
      # plot title
      plot(c(0.00,0.45),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
      lines(c(0.55,1   ),c(1,1),cex=1,lwd=1,type="l",xlim=c(0,1),ylim=c(0,1),xaxt='n', yaxt='n', ann=F)
      text(0.5,1,labels = c("TicTacJoe"), font=1, col=1, cex=1)
      # plot face
      points(0.5 ,0.37,cex=16,lwd=5,pch=1,col="black")
      points(0.5 ,0.47 ,cex=20,lwd=5,pch=15,col="white")
      points(0.5 ,0.4 ,cex=24,lwd=5,pch=1,col="black")
      points(0.47,0.43 ,cex=7,lwd=5,pch=4,col=NiceColorPlayerOne)
      points(0.54,0.43 ,cex=7,lwd=5,pch=1,col=NiceColorPlayerTwo)
      # plot welcome message
      text(0.5,0.7,labels = c("Thank you for playing!"), font=1, col=1, cex=2)
      Sys.sleep(3)
      graphics.off()
    }
  }
}

StartAdventure()
