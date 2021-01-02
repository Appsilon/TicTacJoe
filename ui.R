semanticPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "TicTacJoe.css")
  ),
  
  tags$br(),
  
  div(class = "ui grid centered",
      action_button(input_id = "play_game", label = "Play a game")
  ),
  
  tags$br(),
  tags$br(),
  
  div(class = "ui grid centered two colum",
      action_button(input_id = "train_more", label = "Let TicTacJoe train"),
      action_button(input_id = "flush_training", label = "Flush TicTacJoe's skills")
  ),
  
  modal(
    id = "game_modal",
    target = "play_game",
    header = "Let's play a game!",
    footer = div(class = "ui button gray", "End game"),
    div(class = "ui grid centered three column",
        action_button(input_id = "top_left", label = ""),
        action_button(input_id = "top_middle", label = ""),
        action_button(input_id = "top_right", label = ""),
    ),
    div(class = "ui grid centered three column",
        action_button(input_id = "middle_left", label = ""),
        action_button(input_id = "middle_middle", label = ""),
        action_button(input_id = "middle_right", label = ""),
    ),
    div(class = "ui grid centered three column",
        action_button(input_id = "bottom_left", label = ""),
        action_button(input_id = "bottom_middle", label = ""),
        action_button(input_id = "bottom_right", label = ""),
    )
  ),
  
  tags$br(),
  
  textOutput("TTJLevel")
  
)