server <- function(input, output, session) {

  val <- reactiveValues(
    level_idx = 1,
    run_training = FALSE,
    training_step = 0,
    check_prob_1 = 0.33,
    check_prob_2 = 0.33,
    check_prob_3 = 0.33,

    who_starts = NULL,
    users_move = NULL,
    whose_turn = ""

    # current_boardstate = rep(0,N),
    # unique_current_boardstate = rep(0,N),
    # PathRun = 1,
    # SelectedMoves = runif(N,0,1),
    # move_nr = 1 # move_nr = i-1, since i=2 initially
  )

  ##########################
  # Play a game
  ##########################

  # Who starts modal
  ##########################
  observeEvent(input$user_starts, {
    if (input$user_starts > 0 ) {
      val$who_starts = "user"
      val$whose_turn = val$who_starts
      val$user_code = 1
      removeModal()
    }
  })
  observeEvent(input$TTJ_starts, {
    if (input$TTJ_starts > 0 ) {
      val$who_starts = "TTJ"
      val$whose_turn = val$who_starts
      val$user_code = 2
      removeModal()
    }
  })
  observeEvent(input$random_starts, {
    if (input$random_starts > 0 ) {
      val$who_starts = sample(c("user", "TTJ"), 1)
      val$whose_turn = val$who_starts
      val$user_code = if(val$who_starts == "user") {1} else {2}
      removeModal()
    }
  })
  observeEvent(input$cancel_choice,{
    removeModal()
  })

  # Game modal
  ##########################
  observeEvent(val$who_starts, {
    # Clean game state before the game
    val$users_move = NULL

    val$current_boardstate = rep(0,N)
    val$unique_current_boardstate = rep(0,N)
    val$PathRun = 1
    val$SelectedMoves = runif(N,0,1)
    val$move_nr = 1
    for(tile in BoardTileNames) {
      UpdateButton(WhichButton = tile, toState="default", session)
    }
    # Hide last winner
    HideWinner()

    # Create board for game
    create_modal(modal(
      id = "game_modal",
      header = list(id = "game_modal_header", style = "background: lightgray", p(id="game_header", "Let's play a game!")),
      # content = list(style = "background: lightblue", `data-custom` = "value", "This is an important message!"),
      footer = div(action_button(input_id = "cancel_game", label = "End game")),
      div(class = "ui grid centered three column",
          # div(
          #   action_button(input_id = "top_left", label = ""),
          #   div(style = "position: absolute; margin-top: -20px; margin-left: 10px", span("43%"))
          # ),
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
      ),
      settings = list(c("autofocus", "false"))
    ))
    isolate({
      val$who_starts = NULL
    })
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_game,{
    print("Game cancelled!")
    removeModal()
  })

  observeEvent(input$top_left,{
    if(val$whose_turn == "user"){
      val$users_move = 1
      val$pressed_button = "top_left"
    }
  })
  observeEvent(input$top_middle,{
    if(val$whose_turn == "user"){
      val$users_move = 2
      val$pressed_button = "top_middle"
    }
  })
  observeEvent(input$top_right,{
    if(val$whose_turn == "user"){
      val$users_move = 3
      val$pressed_button = "top_right"
    }
  })
  observeEvent(input$middle_left,{
    if(val$whose_turn == "user"){
      val$users_move = 4
      val$pressed_button = "middle_left"
    }
  })
  observeEvent(input$middle_middle,{
    if(val$whose_turn == "user"){
      val$users_move = 5
      val$pressed_button = "middle_middle"
    }
  })
  observeEvent(input$middle_right,{
    if(val$whose_turn == "user"){
      val$users_move = 6
      val$pressed_button = "middle_right"
    }
  })
  observeEvent(input$bottom_left,{
    if(val$whose_turn == "user"){
      val$users_move = 7
      val$pressed_button = "bottom_left"
    }
  })
  observeEvent(input$bottom_middle,{
    if(val$whose_turn == "user"){
      val$users_move = 8
      val$pressed_button = "bottom_middle"
    }
  })
  observeEvent(input$bottom_right,{
    if(val$whose_turn == "user"){
      val$users_move = 9
      val$pressed_button = "bottom_right"
    }
  })

  # Users move
  ##########################

  observeEvent(val$users_move, {
    if(val$whose_turn == "user"){
      # Update board tile with the users move
      UpdateButton(WhichButton=val$pressed_button, toState="user", session)

      # Make the users move in the backend
       ## Denote move in current_boardstate
      val$current_boardstate[val$users_move] = val$user_code
      print(paste("val$current_boardstate:", paste(val$current_boardstate, collapse = " "), "(user moved)"))

       ## Update PathRun
      unique_current_boardstate = c(GetEquivalentStates(val$current_boardstate)[8,])
      #print(paste("unique_current_boardstate:", paste(unique_current_boardstate, collapse = " ")))
      for (j in 1:length(LinkedStates[[val$move_nr]][[val$PathRun[val$move_nr]]])) {
        if(all(States[[val$move_nr+1]][[LinkedStates[[val$move_nr]][[val$PathRun[val$move_nr]]][j]]] == unique_current_boardstate)) {
          val$PathRun[val$move_nr+1] = LinkedStates[[val$move_nr]][[val$PathRun[val$move_nr]]][j]
          break
        }
      }
      #print(paste("val$PathRun:", paste(val$PathRun, collapse = " ")))

      # Check if game ended
      winner = CheckIfWon(StopStates, val$PathRun, val$move_nr, val$user_code)
      if(is.null(winner)) {
        # TTJs turn
        val$whose_turn = "TTJ"
        val$move_nr = val$move_nr + 1
      } else {
        val$whose_turn = NULL
        # Game ends - display who won
        DisplayWinner(winner)
      }
    }
  })

  # TTJs move
  ##########################

  observeEvent(val$whose_turn, {
    if(val$whose_turn == "TTJ") {
      # Display probabilities of moves
      #print(paste("Level of confidence:", round(max(ProbStates[[val$move_nr]][[val$PathRun[val$move_nr]]])*100, 0),"%"))
      #print(paste("All probabilities are:", paste(round(ProbStates[[val$move_nr]][[val$PathRun[val$move_nr]]], 3), collapse=", ")))
      val$next_moves = 1
      for(i in 1:length(ProbStates[[val$move_nr]][[val$PathRun[val$move_nr]]])) { # loop over possible moves
        prob_next_state = ProbStates[[val$move_nr]][[val$PathRun[val$move_nr]]][[i]]
        next_state = States[[val$move_nr+1]][[LinkedStates[[val$move_nr]][[val$PathRun[val$move_nr]]][[i]]]]
        equivalent_next_states = GetEquivalentStates(next_state)
        # find index of equivalent_next_states with only one change needed to get to current_boardstate
        for(j in 1:8) { # loop over equivalent states
          changes_needed = which(val$current_boardstate != equivalent_next_states[j,])
          if(length(changes_needed) == 1) { # the new move is the only difference
            next_move = BoardTileNames[changes_needed]
            break
          }
        }
        print(paste("move:", next_move, "prob:", paste0(round(prob_next_state * 100), "%")))
        val$next_moves[[i]] = next_move
        runjs(glue('document.getElementById("{next_move}").innerHTML = "{paste0(round(prob_next_state * 100), "%")}";'))
      }
      Sys.sleep(2)
      val$clean_probs = TRUE
    }
  })

  observeEvent(val$clean_probs, {
    for(tile in val$next_moves) {
      update_action_button(session, input_id = tile, label="")
    }
    isolate({
      val$clean_probs = NULL
    })
    val$TTJ_makes_move = TRUE
  })

  observeEvent(val$TTJ_makes_move, {
    if(val$whose_turn == "TTJ") {
      # Make TTJs move in the backend
      Probabilities = cumsum(ProbStates[[val$move_nr]][[val$PathRun[val$move_nr]]])
      val$PathRun[val$move_nr+1] = LinkedStates[[val$move_nr]][[val$PathRun[val$move_nr]]][which(val$SelectedMoves[[val$move_nr]] < Probabilities)[1]]
      unique_current_boardstate = States[[val$move_nr+1]][[val$PathRun[[val$move_nr+1]]]]
      equivalent_states = GetEquivalentStates(unique_current_boardstate)
      previous_boardstate = val$current_boardstate
      val$current_boardstate = equivalent_states[which(rowSums(equivalent_states == t(matrix(rep(val$current_boardstate,8),nrow=N,ncol=8)))==8)[1],]
      print(paste("val$current_boardstate:", paste(val$current_boardstate, collapse = " "), "(TTJ moved)"))
      TTJs_choice = which(previous_boardstate < val$current_boardstate)
      print(paste("TTJ made the move:", TTJs_choice, "so", BoardTileNames[[TTJs_choice]]))

      # Update board tile with TTJs move
      UpdateButton(WhichButton=BoardTileNames[[TTJs_choice]], toState="TTJ", session)

      # Check if game ended
      winner = CheckIfWon(StopStates, val$PathRun, val$move_nr, val$user_code)
      if(is.null(winner)) {
        # TTJs turn
        val$whose_turn = "user"
        val$move_nr = val$move_nr + 1
      } else {
        # Game ends - display who won
        DisplayWinner(winner)
      }
      isolate({
        val$TTJ_makes_move = NULL
      })
    }
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
    print(paste("Probabilites of first TTJs move:", paste(round(ProbStates[[1]][[1]], 3), collapse=" ")))
    print(paste("Temperature:", round(Temperature, 3)))
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
          print(paste("Probabilites of first TTJs move:", paste(round(ProbStates[[1]][[1]],3), collapse=" ")))
          print(paste("Temperature:", round(Temperature, 3)))
        }
      })
      if(isolate(val$training_step) < LengthOfTraining){
        invalidateLater(0, session)
      }
    }
  })

  output$TTJLevel <- renderText(paste("TicTacJoe is a", "<b>", TTJLevels[[val$level_idx]], "</b>"))

  output$move_prob_1 <- renderPlot({
    plot(val$check_prob_1, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="", ylab="Probability of TTJs first move when he starts", main="Pick corner")
    points(val$training_step, tail(val$check_prob_1, n=1), col=NiceColorTTJ, cex=3, pch=19)
  }, width = 500)
  output$move_prob_2 <- renderPlot({
    plot(val$check_prob_2, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="Games trained", ylab="", main="Pick side")
    points(val$training_step, tail(val$check_prob_2, n=1), col=NiceColorTTJ, cex=3, pch=19)
  }, width = 500)
  output$move_prob_3 <- renderPlot({
    plot(val$check_prob_3, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="", ylab="", main="Pick center")
    points(val$training_step, tail(val$check_prob_3, n=1), col=NiceColorTTJ, cex=3, pch=19)
  }, width = 500)

}
