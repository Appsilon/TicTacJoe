server <- function(input, output, session) {
  
  val <- reactiveValues(
    level_idx = 1,
    run_training = FALSE,
    training_step = 0,
    check_prob_1 = 0.33,
    check_prob_2 = 0.33,
    check_prob_3 = 0.33
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
    UpdateButton(WhichButton="top_left", isHuman=TRUE, session)
    # val$player_chose = "top_left"
  })
  observeEvent(input$top_middle,{
    UpdateButton(WhichButton="top_middle", isHuman=TRUE, session)
  })
  observeEvent(input$top_right,{
    UpdateButton(WhichButton="top_right", isHuman=TRUE, session)
  })
  observeEvent(input$middle_left,{
    UpdateButton(WhichButton="middle_left", isHuman=TRUE, session)
  })
  observeEvent(input$middle_middle,{
    UpdateButton(WhichButton="middle_middle", isHuman=TRUE, session)
  })
  observeEvent(input$middle_right,{
    UpdateButton(WhichButton="middle_right", isHuman=TRUE, session)
  })
  observeEvent(input$bottom_left,{
    UpdateButton(WhichButton="bottom_left", isHuman=TRUE, session)
  })
  observeEvent(input$bottom_middle,{
    UpdateButton(WhichButton="bottom_middle", isHuman=TRUE, session)
  })
  observeEvent(input$bottom_right,{
    UpdateButton(WhichButton="bottom_right", isHuman=TRUE, session)
  })

  observe({
    delay(5000, {
      UpdateButton(WhichButton="top_left", isHuman=FALSE, session)
      # reac$tl <- "TTJ"
      delay(5000, {
        UpdateButton(WhichButton="bottom_left", isHuman=FALSE, session)
        # reac$bl <- "TTJ"
      })
    })
  })
  
  
  ##########################
  # Train
  ##########################
  observe({
    if(val$level_idx == 1){
      shinyjs::disable("flush_training")
      shinyjs::enable("train_more")
    } else {
      shinyjs::enable("flush_training")
      if(val$level_idx == 3) {
        shinyjs::disable("train_more")
      }
    }
  })
  
  observeEvent(input$flush_training,{
    ProbStates <<- RandomProbStates
    Temperature <<- InitialTemperature
    val$level_idx = 1
    val$training_step = 0
    val$check_prob_1 = 0.33
    val$check_prob_2 = 0.33
    val$check_prob_3 = 0.33
  })

  observeEvent(input$train_more,{
    val$run_training = TRUE
  })
  observe({
    isolate({
      # This block is scheduled many times - should run steps_in_plot_chunk many steps of training
      if(val$run_training == TRUE){
        for (i in 1:steps_in_plot_chunk) {
          # Run a chunk of training
          ProbStates <<- RunTicTacToeComputerVSComputer(States,StopStates,LinkedStates,ProbStates,Temperature)
          Temperature <<- Temperature + TemperatureDecreaseStep
          val$check_prob_1[val$training_step] <- ProbStates[[1]][[1]][[1]]
          val$check_prob_2[val$training_step] <- ProbStates[[1]][[1]][[2]]
          val$check_prob_3[val$training_step] <- ProbStates[[1]][[1]][[3]]
          val$training_step <- val$training_step + 1
        }
        if(val$training_step %% floor(LengthOfTraining/2) == 0){
          # At end of training level, update the level
          val$level_idx = val$level_idx + 1
          val$run_training = FALSE
          print(ProbStates[[1]][[1]])
          print(Temperature)
        }
      }
    })
    if (isolate(val$training_step) < LengthOfTraining){
      invalidateLater(0, session)
    }
  })
  # observeEvent(input$train_more,{
  #   isolate({
  #     # This block is scheduled many times - should run steps_in_plot_chunk many steps of training
  #     for (i in 1:steps_in_plot_chunk) {
  #       # Run a chunk of training
  #       ProbStates <<- RunTicTacToeComputerVSComputer(States,StopStates,LinkedStates,ProbStates,Temperature)
  #       Temperature <<- Temperature + TemperatureDecreaseStep
  #       val$check_prob[val$training_step + floor(LengthOfTraining/2)*(val$level_idx - 1)] <- ProbStates[[1]][[1]][[1]]
  #       val$training_step <- val$training_step + 1
  #     }
  #     if(val$training_step == floor(LengthOfTraining/2)){
  #       # At end of training level, update the level
  #       val$level_idx = val$level_idx + 1
  #       print(ProbStates[[1]][[1]])
  #       print(Temperature)
  #     }
  #   })
  #   if (isolate(val$training_step) < floor(LengthOfTraining/2)){
  #     invalidateLater(0, session)
  #   }
  # })

  output$TTJLevel <- renderText(paste0("TicTacJoe is a ", TTJLevels[[val$level_idx]]))

  output$move_prob_1 <- renderPlot({
    plot(val$check_prob_1, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="", ylab="Probability", main="Pick corner")
    points(val$training_step, tail(val$check_prob_1, n=1), col=NiceColorPlayerOne, cex=3, pch=19)
  }, width = 500)
  output$move_prob_2 <- renderPlot({
    plot(val$check_prob_2, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="Games trained", ylab="", main="Pick side")
    points(val$training_step, tail(val$check_prob_2, n=1), col=NiceColorPlayerOne, cex=3, pch=19)
  }, width = 500)
  output$move_prob_3 <- renderPlot({
    plot(val$check_prob_3, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="", ylab="", main="Pick center")
    points(val$training_step, tail(val$check_prob_3, n=1), col=NiceColorPlayerOne, cex=3, pch=19)
  }, width = 500)
  
  # observe({
  #   if(val$training_step > 1){
  #     output$move_prob <- renderPlot(plot(check_prob, type='l'))#[seq(1, length(check_prob), 200)], type='l'))     
  #     # output$move_prob_1 <- renderPlot(plot(check_prob[1], type='l'))#[seq(1, length(check_prob), 200)], type='l'))     
  #     # output$move_prob_2 <- renderPlot(plot(check_prob[2], type='l'))#[seq(1, length(check_prob), 200)], type='l'))     
  #     # output$move_prob_3 <- renderPlot(plot(check_prob[3], type='l'))#[seq(1, length(check_prob), 200)], type='l'))     
  #   }
  # })
  # 
}