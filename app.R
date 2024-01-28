
# Setup ----
library(shiny)
library(plotly)
source("scripts/resource_creation.R")

ui <- fillPage(
  uiOutput("visible_screen"),
  tags$head(
    includeCSS("styles.css")
  )
)

# Server ----
server <- function(input, output, session){
  intro_page <- reactiveVal(TRUE)
  host_start <- reactiveVal(FALSE)
  host_wait <- reactiveVal(FALSE)
  join_start <- reactiveVal(FALSE)
  join_wait <- reactiveVal(FALSE)
  game_begun <- reactiveVal(FALSE)
  failed_login <- reactiveVal(FALSE)
  attempted_to_join_nonexistent_game <- reactiveVal(FALSE)
  player_table <- reactiveVal(NULL)

  game_id <- paste(sample(LETTERS, 10), collapse = "")
  game_id <- "ABC"
  game_dir <- paste0("game_files/", game_id, "/")
  dir.create(game_dir)
  saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  out_of_date <- reactiveFileReader(100, session, paste0(game_dir, "out_of_date.rds"), readFunc = readRDS)
  
  observeEvent(out_of_date(), {
    print("Noticed out of date!")
    player_table(readRDS(paste0(game_dir, "player_table.rds")))
    game_begun(readRDS(paste0(game_dir, "game_begun.rds")))
    if(game_begun())join_wait(FALSE)
    print(player_table())
  }, ignoreInit = TRUE)
  
  output$visible_screen <- renderUI({
    if(intro_page()){
      drawIntroPage()
    } else if(host_start()){
      drawHostStartPage()
    } else if(host_wait()){
      drawHostWaitPage(game_id = game_id, player_table = player_table())
    } else if(join_start()){
      drawJoinStartPage()
    } else if(join_wait()){
      drawJoinWaitPage(game_id = game_id, player_table = player_table())
    } else if(attempted_to_join_nonexistent_game()){
      drawFailedJoinPage(fail_type="nonexisting", entered_id = input$game_id_entered,
                         entered_uname=input$join_uname, entered_pwd = input$join_pwd)
    } else if(failed_login()){
      drawFailedJoinPage(fail_type="failed login", entered_id = input$game_id_entered,
                         entered_uname=input$join_uname, entered_pwd = input$join_pwd)
    } else if(game_begun()){
      drawGameboard()
    } else {
      div()
    }
  })
  
  observeEvent(input$new_game_button, {
    intro_page(FALSE)
    host_start(TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$choose_join_game, {
    intro_page(FALSE)
    join_start(TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$host_ready_button, {
    host_start(FALSE)
    
    saveRDS(FALSE, file = paste0(game_dir, "game_begun.rds"))
    player_table(data.frame(uname=input$host_uname, pwd=input$host_pwd))
    saveRDS(player_table(), paste0(game_dir, "player_table.rds"))
    
    # Generate the world table here and save
    # Generate the globe plates here and save
    
    host_wait(TRUE)
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  }, ignoreInit = TRUE)
  observeEvent(input$join_ready_button, {
    join_start(FALSE)
    game_id <- input$game_id_entered
    game_dir <- paste0("game_files/", game_id, "/")
    
    print("New game join attempted")
    attempted_to_join_nonexistent_game(FALSE)
    failed_login(FALSE)
    print(file.exists(paste0(game_dir, "game_begun.rds")))
    
    if(!dir.exists(game_dir)){
      print("Attempted to join nonexistent game")
      attempted_to_join_nonexistent_game(TRUE)
    } else {
      if(readRDS(paste0(game_dir, "game_begun.rds"))){
        player_table <- readRDS(paste0(game_dir, "player_table.rds"))
        if(input$join_uname%in%player_table$uname){
          if(input$join_pwd==player_table$pwd[player_table$uname==input$join_uname]){
            failed_login(FALSE)
            game_begun(TRUE)
          } else {
            failed_login(TRUE)
          }
        } else {
          failed_login(TRUE)
        }
      } else {
        player_table(rbind(
          readRDS(paste0(game_dir, "player_table.rds")), 
          data.frame(uname=input$join_uname, pwd=input$join_pwd))
        )
        saveRDS(player_table(), paste0(game_dir, "player_table.rds"))
        saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
        
        # Read in world table
        # Read in globe plates
        
        join_wait(TRUE)
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$game_start, {
    host_wait(FALSE)
    join_wait(FALSE)
    game_begun(TRUE)
    
    saveRDS(TRUE, paste0(game_dir, "game_begun.rds"))
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
    
  }, ignoreInit = TRUE)
}

drawIntroPage <- function(){
  div(
    class = "center-both",
    wellPanel(
      h3("Welcome to Planetan!"),
      actionButton("new_game_button", "Host new game", width = "100%"),
      actionButton("choose_join_game", "Join existing", width = "100%"),
    )
  )
}
drawHostStartPage <- function(){
  div(
    class = "center-both",
    wellPanel(
      h3("Start a new game"),
      textInput("host_uname", label = "Pick a username:", width = "100%", value = "admin"),
      textInput("host_pwd", label = "Pick your passcode:", width = "100%", value = "password"),
      actionButton("host_ready_button", "Start hosting", width = "100%")
    )
  )
}
drawHostWaitPage <- function(game_id, player_table){
  div(
    class = "center-both",
    wellPanel(
      h3("Waiting for players"),
      h3(paste("Game ID:", game_id)),
      h3(paste("Current players:", paste(player_table$uname, collapse = ", "))),
      actionButton("game_start", "Start game!", width = "100%")
    )
  )
}
drawJoinStartPage <- function(){
  div(
    class = "center-both",
    wellPanel(
      textInput("game_id_entered", label = "Enter Game ID:", width = "100%", value = "ABC"),
      textInput("join_uname", label = "Pick a username:", width = "100%", value = "newplayer"),
      textInput("join_pwd", label = "Pick your passcode:", width = "100%", value = "alsopwd"),
      actionButton("join_ready_button", "Join game", width = "100%")
    )
  )
}
drawJoinWaitPage <- function(game_id, player_table){
  div(
    class = "center-both",
    wellPanel(
      h3("Waiting for players"),
      h3(paste("Game ID:", game_id)),
      h3(paste("Current players:", paste(player_table$uname, collapse = ", ")))
    )
  )
}
drawFailedJoinPage <- function(fail_type, entered_id, entered_uname, entered_pwd){
  wellpanel_divs <- tagList(
    textInput("game_id_entered", label = "Enter Game ID:", width = "100%", value = entered_id),
    textInput("join_uname", label = "Pick a username:", width = "100%", value = entered_uname),
    textInput("join_pwd", label = "Pick your passcode:", width = "100%", value = entered_pwd),
    actionButton("join_ready_button", "Join game", width = "100%")
  )
  if(fail_type=="nonexisting"){
    wellpanel_divs <- c(wellpanel_divs, tagList(
      p("That Game ID does not yet exist.", style="color:red;")
    ))
  }
  if(fail_type=="failed login"){
    wellpanel_divs <- c(wellpanel_divs, tagList(
      p("Unable to find login info for that Game ID.", style="color:red;")
    ))
  }
  div(
    class = "center-both",
    wellPanel(
      wellpanel_divs
    )
  )
}

drawGameboard <- function(){
  tagList(
    sidebarPanel(
      uiOutput("whoami"),
      uiOutput("presentcurrentplayer"),
      h3("Resources available:"),
      tableOutput("presentplayerresources"),
      uiOutput("devcarddetails"),
      uiOutput("buydevcard"),
      uiOutput("rolldicebutton"),
      uiOutput("endturnbutton"),
      checkboxInput(inputId = "showpipvalues", label = "Show pip values?", value = TRUE),
      checkboxInput(inputId = "debug", label = "Debug enabled?", value = TRUE),
      textOutput("playervalues")
    ),
    mainPanel(
      plotlyOutput("world", height = "100vh")
    )
  )
}

# Run app ----
shinyApp(ui, server)