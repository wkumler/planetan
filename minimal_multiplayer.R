
library(shiny)

player_list <- c("a", "b")
saveRDS("a", "current_player.rds")
saveRDS("nobody", "all_player_wins.rds")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("my_id", label = "Player ID", value = "a"),
      uiOutput("nextbutton")
    ),
    mainPanel(
      textOutput("winnerlist")
    )
  )
)

server <- function(input, output, session){
  current_player <- reactiveFileReader(
    intervalMillis = 100, 
    filePath = 'current_player.rds', 
    readFunc = readRDS, 
    session = session
  )
  all_player_wins <- reactiveFileReader(
    intervalMillis = 100, 
    filePath = 'all_player_wins.rds', 
    readFunc = readRDS, 
    session = session
  )
  output$nextbutton <- renderUI({
    if(current_player()==input$my_id){
      actionButton("next_turn", label = "End my turn")
    } else {
      div()
    }
  })
  output$winnerlist <- renderText({
    all_player_wins()
  })
  observeEvent(input$next_turn, {
    player_to_save <- ifelse(current_player()=="a", "b", "a")
    saveRDS(player_to_save, "current_player.rds")
    saveRDS(c(all_player_wins(), current_player()), 'all_player_wins.rds')
  })
}

browseURL("http://127.0.0.1:5013/")
shinyApp(ui, server, options = list(launch.browser=TRUE, port=5013))
