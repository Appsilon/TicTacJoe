semanticPage(
  suppress_bootstrap = TRUE,
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "TicTacJoe.css")
  ),
  
  tags$br(),
  tags$br(),
  tags$br(),
  
  div(class = "ui grid centered",
      div(style = "padding: 0px 0px;",
        span("Welcome to"), span("TicTacJoe", class="TTJ_font"), span(","),
        p("a Reinforcement Learning agent which you can train to master the game of Tic Tac Toe.")
      )  
  ),
  
  tags$br(),
  tags$br(),
  tags$br(),
  
  div(class = "ui grid centered",
      htmlOutput("TTJLevel")
  ),  
  
  tags$br(),
  tags$br(),
  tags$br(),
  
  div(class = "ui grid centered",
    column(
      3,
      action_button(input_id = "play_game", label = span(tagList(icon("gamepad"), "Play a game")))
    ),
    column(
      5,
      action_button(input_id = "train_more", label = span(tagList(icon("dumbbell"), "Let TicTacJoe train"))),
      action_button(input_id = "flush_training", label = span(tagList(icon("wind"), "Flush TicTacJoe's skills")))
    ),
    column(
      3,
      action_button(input_id = "learn_more", label = span(tagList(icon("question"), "Learn more")))
    )
  ),
  
  modal(
    id = "learn_more_modal",
    target = "learn_more",
    header = "Who is TicTacJoe?",
    footer = div(action_button(input_id = "cancel_learn_more", label = "Close")),
    p(strong("TicTacJoe"), "is an agent playing the game of Tic Tac Toe."),
    p("You can play against TicTicJoe by pressing the", strong('Play a game'), "button.
      When it is your turn, click a tile to make the move. 
      When TicTacJoe is moving you will see the likelihood of him choosing a certain tile displayed on the tiles.
      Note that some tiles are equivalent (symmetric) and hence don't get their separate likelihood."),
    p("Initially, TicTacJoe performs each move randomly (all likelihoods are equivalent in a given turn), so he is a", strong("Noob"), "in the game.
      However, you can let TicTacJoe become better at the game, by letting him play against himself multiple times (click the", strong('Let TicTacJoe train'),"button).
      Observe, how he gradually becomes a Tic Tac Toe", strong("Guru"), ":)"),
    p("The three graphs at the bottom visualize the evolution of the likelihoods of the first three possible moves TicTacJoe can make when he starts a game.
      They start from being equal (33% each), and as the training progresses, freeze out to always choosing the optimal move."),
    p("Interestingly, TicTacJoe's brain is implemented explicitly in pure R (with no extra libraries). 
      It is an example of reinforcement learning, since as he plays against himself, he gets rewarded for moves leading him to win and hence encouraged to make them in the future.
      Similarly, he gets penalized for those leading to his loss, and hence makes them less often.
      To encourage the initial exploration, and exploitation in the later stages of training, a temperature-like mechanism is introduced.
      Those mechanisms turn out to be enough to guarantee TicTacJoe almost always performs one of the optimal moves when trained to be a Guru.")
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
    
  div(id="likelihood_graphs",
    class = "ui grid centered",
    style = "visibility: hidden;",
    split_layout(
      plotOutput("move_prob_1"),
      plotOutput("move_prob_2"),
      plotOutput("move_prob_3"),
      style = "background-color: default;"
    )
  ),
  
)