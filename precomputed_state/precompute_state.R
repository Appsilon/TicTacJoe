
# # Initialize needed parameters/variables
N = 9
n = sqrt(N)
p = 3

# Initialize the objects storing State of TicTacJoe (probabilities of next moves, allowed next moves etc.)
States = list()
StopStates = list()
LinkedStates = list()
ProbStates = list()
for (i in 1:(N+1)) {
  States[[i]] = list()
  StopStates[[i]] = list()
  LinkedStates[[i]] = list()
  ProbStates[[i]] = list()
}
States[[1]][[1]] = numeric(N)
StopStates[[1]][[1]] = 0

# Calculate all possible valid board states, taking into account rotating and mirroring.
# A state is defined by a vector of length 9, where 0=empty, 1=player1, 2=player2
# This example vector [0 1 2 0 2 1 2 0 0] can be plotted as the following board where player 1 wins
#  | - | O | x |
#  | - | x | O |
#  | x | - | - |

# Create all possible valid board states (excluding rotation/mirroring) of the game TicTacToe
for (k in 2:(N+1)) { # Number of levels deep into the TicTacToe game
  player = (k %% 2) + 1 # Which player is making his/her move
  for (i in 1:length(States[[k-1]])) { # Number of states at previous level
    for (j in 1:N) { # Fill in the square located at this position
      if (States[[k-1]][[i]][j]==0 && StopStates[[k-1]][[i]]==0) {
        current_states = matrix(0,nrow=8,ncol=N) # Number of similarly rotated and/or Mirrored state
        current_states[1,1:N] = States[[k-1]][[i]]
        current_states[1,j] = player
        current_states[2,1:N] = c(apply(t(matrix(current_states[1,1:N],nrow=n,ncol=n)), 2, rev))
        current_states[3,1:N] = c(apply(t(matrix(current_states[2,1:N],nrow=n,ncol=n)), 2, rev))
        current_states[4,1:N] = c(apply(t(matrix(current_states[3,1:N],nrow=n,ncol=n)), 2, rev))
        current_states[5,1:N] = c(apply(matrix(current_states[1,1:N],nrow=n,ncol=n), 2, rev))
        current_states[6,1:N] = c(apply(matrix(current_states[2,1:N],nrow=n,ncol=n), 2, rev))
        current_states[7,1:N] = c(apply(matrix(current_states[3,1:N],nrow=n,ncol=n), 2, rev))
        current_states[8,1:N] = c(apply(matrix(current_states[4,1:N],nrow=n,ncol=n), 2, rev))
        current_states = current_states[do.call(order, as.data.frame(current_states)),]
        unique_state = c(current_states[8,])
        new_state = 1
        a = 0
        if (length(States[[k]])>0) {
          for (a in 1:length(States[[k]])) {
            if (all(unique_state == States[[k]][[a]])) {
              new_state = 0
            }
          }
        }
        if (new_state == 1) {
          States[[k]][[a+1]] = unique_state
          matrix_unique_state = matrix(unique_state,nrow=n,ncol=n)
          StopStates[[k]][[a+1]] = 0
          for (z1 in 1:n) {
            for (z2 in 1:(n-p+1)) {
              if (all(matrix_unique_state[z1,z2:(z2+p-1)] == rep(1,p)) ||
                  all(matrix_unique_state[z2:(z2+p-1),z1] == rep(1,p))) {
                StopStates[[k]][[a+1]] = 1
              }
              if (all(matrix_unique_state[z1,z2:(z2+p-1)] == rep(2,p)) ||
                  all(matrix_unique_state[z2:(z2+p-1),z1] == rep(2,p))) {
                StopStates[[k]][[a+1]] = 2
              }
              if (z1 <= (n-p+1) && (
                all(c(matrix_unique_state[z1,z2], matrix_unique_state[z1+1,z2+1],
                      matrix_unique_state[z1+2,z2+2]) == rep(1,3)) ||
                all(c(matrix_unique_state[z1,n-z2+1], matrix_unique_state[z1+1,n-z2+1-1],
                      matrix_unique_state[z1+2,n-z2+1-2]) == rep(1,3)))) {
                StopStates[[k]][[a+1]] = 1
              }
              if (z1 <= (n-p+1) && (
                all(c(matrix_unique_state[z1,z2], matrix_unique_state[z1+1,z2+1],
                      matrix_unique_state[z1+2,z2+2]) == rep(2,3)) ||
                all(c(matrix_unique_state[z1,n-z2+1], matrix_unique_state[z1+1,n-z2+1-1],
                      matrix_unique_state[z1+2,n-z2+1-2]) == rep(2,3)))) {
                StopStates[[k]][[a+1]] = 2
              }
            }
          }
        }
      }
    }
  }
}

# Create the Links between all state combinations
LinkedStates[[1]][[1]] = 1:length(States[[2]])
for (k in 2:(N+1)) { # Number of levels deep into the TicTacToe game
  player = ((k+1) %% 2) + 1 # Which player is making the next move
  for (i in 1:length(States[[k]])) { # Number of states at level k
    next_states = t(replicate(N, States[[k]][[i]])) # Possible future 1-step states
    LinkedStates[[k]][[i]] = 0
    for (j in 1:N) { # Fill in the square located at this position
      if (States[[k]][[i]][j]==0 && StopStates[[k]][[i]]==0) {
        next_states[j,j] = player
        similar_next_states = matrix(0,nrow=8,ncol=N) # Number of similarly rotated and/or Mirrored state
        similar_next_states[1,1:N] = next_states[j,]
        similar_next_states[2,1:N] = c(apply(t(matrix(similar_next_states[1,1:N],nrow=n,ncol=n)), 2, rev))
        similar_next_states[3,1:N] = c(apply(t(matrix(similar_next_states[2,1:N],nrow=n,ncol=n)), 2, rev))
        similar_next_states[4,1:N] = c(apply(t(matrix(similar_next_states[3,1:N],nrow=n,ncol=n)), 2, rev))
        similar_next_states[5,1:N] = c(apply(matrix(similar_next_states[1,1:N],nrow=n,ncol=n), 2, rev))
        similar_next_states[6,1:N] = c(apply(matrix(similar_next_states[2,1:N],nrow=n,ncol=n), 2, rev))
        similar_next_states[7,1:N] = c(apply(matrix(similar_next_states[3,1:N],nrow=n,ncol=n), 2, rev))
        similar_next_states[8,1:N] = c(apply(matrix(similar_next_states[4,1:N],nrow=n,ncol=n), 2, rev))
        similar_next_states = similar_next_states[do.call(order, as.data.frame(similar_next_states)),]
        unique_next_state = c(similar_next_states[8,])
        if (k <= N) {
          for (m in 1:length(States[[k+1]])) {
            future_states = matrix(0,nrow=8,ncol=N) # Number of similarly rotated and/or Mirrored state
            future_states[1,1:N] = States[[k+1]][[m]]
            future_states[2,1:N] = c(apply(t(matrix(future_states[1,1:N],nrow=n,ncol=n)), 2, rev))
            future_states[3,1:N] = c(apply(t(matrix(future_states[2,1:N],nrow=n,ncol=n)), 2, rev))
            future_states[4,1:N] = c(apply(t(matrix(future_states[3,1:N],nrow=n,ncol=n)), 2, rev))
            future_states[5,1:N] = c(apply(matrix(future_states[1,1:N],nrow=n,ncol=n), 2, rev))
            future_states[6,1:N] = c(apply(matrix(future_states[2,1:N],nrow=n,ncol=n), 2, rev))
            future_states[7,1:N] = c(apply(matrix(future_states[3,1:N],nrow=n,ncol=n), 2, rev))
            future_states[8,1:N] = c(apply(matrix(future_states[4,1:N],nrow=n,ncol=n), 2, rev))
            future_states = future_states[do.call(order, as.data.frame(future_states)),]
            unique_future_state = c(future_states[8,])
            if (all(unique_next_state == unique_future_state)) {
              LinkedStates[[k]][[i]] = c(LinkedStates[[k]][[i]],m)
              break;
            }
          }
        }
      }
    }
  }
}

# Initialize completely random Markov chain model + make Links unique
for (k in 1:N) { # Number of levels deep into the TicTacToe game
  player = ((k+1) %% 2) + 1 # Which player is making his/her move
  for (i in 1:length(States[[k]])) { # Number of states at current level
    possible_next_states = unique(LinkedStates[[k]][[i]])
    possible_next_states = possible_next_states[possible_next_states!=0]
    ProbStates[[k]][[i]] = rep(1/length(possible_next_states),length(possible_next_states))
    if (length(ProbStates[[k]][[i]]) == 0) {
      ProbStates[[k]][[i]] = 1
    }
    LinkedStates[[k]][[i]] = possible_next_states
  }
}
RandomProbStates = ProbStates  # this stores the untrained TicTacJoe


# Save precomputed game setup
save(States, file="States.RData")
save(StopStates, file="StopStates.RData")
save(LinkedStates, file="LinkedStates.RData")
save(ProbStates, file="ProbStates.RData")


# # Load precomputed game setup
# load(file="States.RData")
# load(file="StopStates.RData")
# load(file="LinkedStates.RData")
# load(file="ProbStates.RData")

