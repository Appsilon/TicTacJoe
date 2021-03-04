semanticPage(
  suppress_bootstrap = TRUE,
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "TicTacJoe.css")
  ),
  
  tags$br(),
  
  div(class = "ui grid centered",
      p("Welcome to TicTacJoe, a Reinforcement Learning agent which you can train to master the game of Tic Tac Toe."),
      p("You can play against TicTicJoe by pressing the 'Play a game' button below.
        When it is your turn, click a tile to make the move. 
        When TicTacJoe is moving you will see the likelihood of him choosing a certain tile displayed on the tiles.
        Note that some tiles are equivalent (symmetric) and hence don't get their separate likelihood."),
      p("Initially, TicTacJoe performs each move randomly (all likelihoods are equivalent in a given turn), so is a noob in the game.
        However, you can let TicTacJoe become better at the game, by letting him play against himself multiple times (click the 'Let TicTacJoe train' button).
        Observe, how he gradually becomes a Tic Tac Toe guru :)"),
      p("The three graphs at the bottom visualize the evolution of the likelihoods of the first three possible moves TicTacJoe can make when he starts a game.
        They start from being equal (33% each), and as the training progresses, freeze out to always choosing the optimal move."),
      p("To find out more about how TicTacJoe's brain works, ", a(href = 'https://appsilon.com', "read the accompanying blogpost", .noWS = "outside"), ". Note, that the TicTacJoe's brain has been written in pure R, with no external libraries used.") 
  ),
  
  tags$br(),
  tags$br(),
  tags$br(),
  
  div(class = "ui grid centered",
      action_button(input_id = "play_game", label = "Play a game")
  ),
  
  tags$br(),
  tags$br(),
  tags$br(),
  
  div(class = "ui grid centered",
      htmlOutput("TTJLevel")
  ),  
  
  div(class = "ui grid centered two column",
      action_button(input_id = "train_more", label = "Let TicTacJoe train"),
      action_button(input_id = "flush_training", label = "Flush TicTacJoe's skills")
  ),
  
  modal(
    id = "whostarts_modal",
    target = "play_game",
    header = "Choose who starts",
    footer = div(action_button(input_id = "cancel_choice", label = "Cancel")),
    div(class = "ui grid centered three column",
        action_button(input_id = "user_starts", label = "I want to start"),
        action_button(input_id = "TTJ_starts", label = "TicTacJoe starts"),
        action_button(input_id = "random_starts", label = "Random option"),
    ),
    settings = list(c("autofocus", "false"))
  ),
  
  tags$br(),
  tags$br(),
    
  div(class = "ui grid centered",
      split_layout(
        plotOutput("move_prob_1"),
        plotOutput("move_prob_2"),
        plotOutput("move_prob_3")
      )
  ),
  
)