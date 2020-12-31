server <- function(input, output, session) {
  
  val <- reactiveValues(
    level_idx = 1
  )
  
  # reac <- reactiveValues(
  #   # Board state
  #   tl = NULL,
  #   tm = NULL,
  #   tr = NULL,
  #   ml = NULL,
  #   mm = NULL,
  #   mr = NULL,
  #   bl = NULL,
  #   bm = NULL,
  #   br = NULL
  #   
    # is it TicTacJoes move
    # MoveTTJ = NULL
  # )
  
  
  ##########################
  # Play a game
  ##########################
  # observeEvent(input$play_game, {
  #   #GameIsOn = TRUE
  #   
  #   # Select who starts (for now random)
  #   HumanPlayer <<- sample(2,1)
  #   ComputerPlayer <<- 3 - HumanPlayer
  #   
  #   # Setup parameters
  #   N2 <<- length(ProbStates)-1
  #   Path_Run <<- 1
  #   Selected_Moves <<- runif(N,0,1)
  #   Probabilities <<- cumsum(ProbStates[[1]][[1]])
  #   current_boardstate <<- rep(0,N)
  #   unique_current_boardstate <<- current_boardstate
  #   i <<- 2 # helps determine whose move it is
  #   if (((i %% 2) + 1) == HumanPlayer) {
  #     # The User makes a move
  #   } else {
  #     # TicTacJoe moves
  #     reac$MoveTTJ = TRUE
  #   }
  # })
  # 
  # 
  # 
  # observeEvent(reac$MoveTTJ, {
  #   if(reac$MoveTTJ){
  #     # Make the move in the back-end
  #     
  #     # Adjust display of the move
  #     runjs(glue('document.getElementById("top_middle").style.backgroundColor = "{NiceColorPlayerOne}";'))
  #     update_action_button(session, input_id = "top_middle", icon = icon("circle outline"))
  #     
  #     # Store for display
  #     reac$tm <- "User"
  #     i <<- i + 1 # next move
  #     # Trigger TicTacJoes move
  #     reac$MoveTTJ = TRUE
  #   }
  # })

  # observe({
  #   current_state <- purrr::map(c("tl", "tm", "tb"), ~{
  #     sel_butt[[.x]]
  #   })
  #   names(current_state) <- c("tl", "tm", "tb")
  #   
  #   print(current_state)
  #   
  #   runjs(glue('document.getElementById("top_left").style.backgroundColor = "{NiceColorPlayerOne}";'))
  #   update_action_button(session, input_id = "top_left", icon = icon("circle outline"))
  #   
  # })

  
  # observeEvent(input$top_left, {
  #   if(is.null(reac$tl) && ((i %% 2) + 1) == HumanPlayer){
  #     UpdateButton("top_left", isHuman=TRUE)
  #     reac$tl <- "User"
  #     i <<- i + 1 # next move
  #   }
  # })
  # observeEvent(input$top_middle, {
  #   if(is.null(reac$tm) && ((i %% 2) + 1) == HumanPlayer){
  #     # Adjust display of the move
  #     runjs(glue('document.getElementById("top_middle").style.backgroundColor = "{NiceColorPlayerOne}";'))
  #     update_action_button(session, input_id = "top_middle", icon = icon("circle outline"))
  #     # Make the move in the back-end
  #     
  #     # Store for display
  #     reac$tm <- "User"
  #     i <<- i + 1 # next move
  #     # Trigger TicTacJoes move
  #     reac$MoveTTJ = TRUE
  #     
  #   }
  # })
  
  observeEvent(input$top_left,{
    UpdateButton(WhichButton="top_left", isHuman=TRUE)
  })
  observeEvent(input$top_middle,{
    UpdateButton(WhichButton="top_middle", isHuman=TRUE)
  })
  observeEvent(input$top_right,{
    UpdateButton(WhichButton="top_right", isHuman=TRUE)
  })
  observeEvent(input$middle_left,{
    UpdateButton(WhichButton="middle_left", isHuman=TRUE)
  })
  observeEvent(input$middle_middle,{
    UpdateButton(WhichButton="middle_middle", isHuman=TRUE)
  })
  observeEvent(input$middle_right,{
    UpdateButton(WhichButton="middle_right", isHuman=TRUE)
  })
  observeEvent(input$bottom_left,{
    UpdateButton(WhichButton="bottom_left", isHuman=TRUE)
  })
  observeEvent(input$bottom_middle,{
    UpdateButton(WhichButton="bottom_middle", isHuman=TRUE)
  })
  observeEvent(input$bottom_right,{
    UpdateButton(WhichButton="bottom_right", isHuman=TRUE)
  })

  observe({
    delay(5000, {
      UpdateButton(WhichButton="top_left", isHuman=FALSE)
      # reac$tl <- "TTJ"
      delay(5000, {
        UpdateButton(WhichButton="bottom_left", isHuman=FALSE)
        # reac$bl <- "TTJ"
      })
    })
  })
  
  
  ##########################
  # Train
  ##########################
  observeEvent(input$train_more,{
    if(val$level_idx < length(TTJLevels)){
      val$level_idx = val$level_idx + 1
    }
  })

  output$TTJLevel <- renderText(paste0("TicTacJoe is a ", TTJLevels[[val$level_idx]]))
    
}
