server <- function(session, input, output) {
  
  sel_butt <- reactiveValues(tl = NULL,
                             tm = NULL,
                             tb = NULL)
  
  
  observe({
    current_state <- purrr::map(c("tl", "tm", "tb"), ~{
      sel_butt[[.x]]
    })
    names(current_state) <- c("tl", "tm", "tb")
    
    print(current_state)
    
    runjs(glue('document.getElementById("top_left").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "top_left", icon = icon("circle outline"))
    
  })
  
  
  observeEvent(input$top_middle,{
    runjs(glue('document.getElementById("top_middle").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "top_middle", icon = icon("circle outline"))
  })
  observeEvent(input$top_right,{
    runjs(glue('document.getElementById("top_right").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "top_right", icon = icon("circle outline"))
  })
  observeEvent(input$middle_left,{
    runjs(glue('document.getElementById("middle_left").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "middle_left", icon = icon("circle outline"))
  })
  observeEvent(input$middle_middle,{
    runjs(glue('document.getElementById("middle_middle").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "middle_middle", icon = icon("circle outline"))
  })
  observeEvent(input$middle_right,{
    runjs(glue('document.getElementById("middle_right").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "middle_right", icon = icon("circle outline"))
  })
  observeEvent(input$bottom_left,{
    runjs(glue('document.getElementById("bottom_left").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "bottom_left", icon = icon("circle outline"))
  })
  observeEvent(input$bottom_middle,{
    runjs(glue('document.getElementById("bottom_middle").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "bottom_middle", icon = icon("circle outline"))
  })
  observeEvent(input$bottom_right,{
    runjs(glue('document.getElementById("bottom_right").style.backgroundColor = "{NiceColorPlayerOne}";'))
    update_action_button(session, input_id = "bottom_right", icon = icon("circle outline"))
  })
  
  observe({
    delay(5000, {
      runjs(glue('document.getElementById("top_left").style.backgroundColor = "{NiceColorPlayerTwo}";'))
      update_action_button(session, input_id = "top_left", icon = icon("close icon"))
      delay(5000, {
        runjs(glue('document.getElementById("bottom_left").style.backgroundColor = "{NiceColorPlayerTwo}";'))
        update_action_button(session, input_id = "bottom_left", icon = icon("close icon"))
      })
    })
  })
  
}