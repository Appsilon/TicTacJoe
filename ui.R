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
    )
  ),
  
  # modal(
  #   id = "game_modal",
  #   target = "start_game",
  #   header = "Let's play a game!",
  #   footer = div(action_button(input_id = "cancel_game", label = "End game")),
  #   div(class = "ui grid centered three column",
  #       action_button(input_id = "top_left", label = ""),
  #       action_button(input_id = "top_middle", label = ""),
  #       action_button(input_id = "top_right", label = ""),
  #   ),
  #   div(class = "ui grid centered three column",
  #       action_button(input_id = "middle_left", label = ""),
  #       action_button(input_id = "middle_middle", label = ""),
  #       action_button(input_id = "middle_right", label = ""),
  #   ),
  #   div(class = "ui grid centered three column",
  #       action_button(input_id = "bottom_left", label = ""),
  #       action_button(input_id = "bottom_middle", label = ""),
  #       action_button(input_id = "bottom_right", label = ""),
  #   )
  # ),
  
  tags$br(),
  tags$br(),
    

  div(class = "ui grid centered",
      split_layout(
        plotOutput("move_prob_1"),
        plotOutput("move_prob_2"),
        plotOutput("move_prob_3")
      )
  ),    
  
  # div(class = "ui grid centered three column",
  #     plotOutput("move_prob_1"),
  #     plotOutput("move_prob_2"),
  #     plotOutput("move_prob_3")
  # ),
  
  
)