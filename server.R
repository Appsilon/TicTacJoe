server <- function(input, output, session) {
  
  val <- reactiveValues(
    level_idx = 1,
    run_training = FALSE,
    training_step = 0,
    check_prob_1 = 0.33,
    check_prob_2 = 0.33,
    check_prob_3 = 0.33,
    
    who_starts = NULL
  )
  
  ##########################
  # Play a game
  ##########################

  # Who starts modal
  ##########################
  observeEvent(input$user_starts, {
    if (input$user_starts > 0 ) {
      val$who_starts = "user"
      removeModal()
    }
  })
  observeEvent(input$TTJ_starts, {
    if (input$TTJ_starts > 0 ) {
      val$who_starts = "TTJ"
      removeModal()
    }
  })
  observeEvent(input$random_starts, {
    if (input$random_starts > 0 ) {
      val$who_starts = sample(c("user", "TTJ"), 1)
      removeModal()
    }
  })
  observeEvent(input$cancel_choice,{
    removeModal()
  })
  
  # Game modal
  ##########################
  observeEvent(val$who_starts, {
    create_modal(modal(
      id = "game_modal",
      header = list(style = "background: lightgray", "Let's play a game!"),
      # content = list(style = "background: lightblue", `data-custom` = "value", "This is an important message!"),
      footer = div(action_button(input_id = "cancel_game", label = "End game")),
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
    ))
    print(val$who_starts)
    val$who_starts = NULL
  }, ignoreInit = TRUE)
  
  observeEvent(input$cancel_game,{
    removeModal()
    # TODO reset the state of the game
  })
  
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
    if(val$run_training == TRUE){
      isolate({
      # This block is scheduled many times - should run steps_in_plot_chunk many steps of training
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
      })
      if(isolate(val$training_step) < LengthOfTraining){
        invalidateLater(0, session)
      }
    }
  })
 
  output$TTJLevel <- renderText(paste("TicTacJoe is a", "<b>", TTJLevels[[val$level_idx]], "</b>"))

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
  
}