
library(shiny)
library(plotly)
source("scripts/resource_creation.R")
options(browser=r"(C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe)")

ui <- fillPage(
  uiOutput("visible_screen"),
  includeCSS("styles.css")
)

server <- function(input, output, session){
  getGameData <- function(rds_obj_name){
    print(paste("Reading", rds_obj_name, "in from file"))
    value <- readRDS(paste0("game_files/", input$game_id, "/", rds_obj_name, ".rds"))
    print(paste("Value:", value))
    return(value)
  }
  setGameData <- function(rds_obj_name, value){
    print(paste("Saving", rds_obj_name, "into file"))
    saveRDS(value, paste0("game_files/", input$game_id, "/", rds_obj_name, ".rds"))
    print(paste("Value:", value))
    return(NULL)
  }
  
  login_status <- reactiveVal("startup")
  init_player_list <- reactiveVal(NULL)
  game_status <- reactiveVal(function(){})
  
  output$visible_screen <- renderUI({
    if(login_status()=="startup"){
      print("Returning startup_div")
      startup_div <- div(
        class = "center-both",
        wellPanel(
          h3("Welcome to Planetan!"),
          actionButton("new_game_button", "Host new game"),
          actionButton("join_game_button", "Join existing"),
        )
      )
      return(startup_div)
    }
    if(login_status()=="make_new_game"){
      print("Returning make_new_game_div")
      taken_game_ids <- readRDS("game_files/existing_game_ids.rds")
      suggested_game_id <- paste0(sample(LETTERS, 6, replace = TRUE), collapse = "")
      while(suggested_game_id%in%taken_game_ids){
        suggested_game_id <- paste0(sample(LETTERS, 6, replace = TRUE), collapse = "")
      }
      make_new_game_div <- div(
        class = "center-both",
        wellPanel(
          h3("Start a new game"),
          textInput("uname", label = "Pick a username:", value = "admin"),
          textInput("pwd", label = "Pick your password:", value = "password"),
          textInput("game_id", label = "Choose a game ID:", value = suggested_game_id),
          actionButton("host_ready_button", "Start hosting")
        )
      )
      return(make_new_game_div)
    }
    if(login_status()=="game_id_taken"){
      print("Returning game_id_taken_div")
      taken_game_ids <- readRDS("game_files/existing_game_ids.rds")
      suggested_game_id <- paste0(sample(LETTERS, 6, replace = TRUE), collapse = "")
      # Make sure we aren't suggesting an already-taken game ID
      while(suggested_game_id%in%taken_game_ids){
        suggested_game_id <- paste0(sample(LETTERS, 6, replace = TRUE), collapse = "")
      }
      game_id_taken_div <- div(
        class = "center-both",
        wellPanel(
          h3("Start a new game"),
          textInput("uname", label = "Pick a username:", value = "admin"),
          textInput("pwd", label = "Pick your password:", value = "password"),
          textInput("game_id", label = "Choose a game ID:", value = suggested_game_id),
          p("That game ID is already taken! Choose another.", style="color:red;"),
          actionButton("host_ready_button", "Start hosting")
        )
      )
      return(game_id_taken_div)
    }
    if(login_status()=="join_existing_game"){
      print("Returning join_existing_game_id_div")
      join_existing_game_div <- div(
        class = "center-both",
        wellPanel(
          h3("Join an existing game"),
          textInput("game_id", label = "Choose a game ID:"),
          actionButton("attempt_join_button", paste("Find game"))
        )
      )
      return(join_existing_game_div)
    }
    if(login_status()=="game_id_not_found"){
      print("Returning game_id_not_found_div")
      game_id_not_found_div <- div(
        class = "center-both",
        wellPanel(
          h3("Join an existing game"),
          textInput("game_id", label = "Choose a game ID:"),
          p("Game ID not found! Try again or start a new game.", style="color:red;"),
          actionButton("attempt_join_button", paste("Find game")),
          actionButton("new_game_button", "Host new game")
        )
      )
      return(game_id_not_found_div)
    }
    if(login_status()=="found_game_id"){
      # Can use input$game_id here
      print("Returning found_game_id_div")
      found_game_id_div <- div(
        class = "center-both",
        wellPanel(
          h3("Provide a username and password"),
          textInput("uname", label = "Username:", value = "newplayer"),
          textInput("pwd", label = "Password:", value = "alsopassword"),
          actionButton("provide_login_info", "Join game")
        )
      )
      return(found_game_id_div)
    }
    if(login_status()=="join_game_failed"){
      print("Returning join_game_failed_div")
      join_game_failed_div <- div(
        class = "center-both",
        wellPanel(
          h3("Provide a username and password"),
          textInput("uname", label = "Username:", value = "newplayer"),
          textInput("pwd", label = "Password:", value = "alsopassword"),
          p("Login failed! Try again or start a new game.", style="color:red;"),
          actionButton("provide_login_info", "Join game"),
          actionButton("new_game_button", "Host new game")
        )
      )
      return(join_game_failed_div)
    }
    
    # If we make it this far then game_id is definitely available as input$game_id
    # and therefore we can read from the game files
    # Also, game_status()() should be defined so we can use that too.
    print(paste("Initial game status read:", game_status()()))
    if(login_status()=="host_waiting"){
      print("Returning host_waiting_div")
      host_waiting_div <- div(
        class = "center-both",
        wellPanel(
          h3("Waiting for players"),
          h3(paste("Game ID:", input$game_id)),
          h3(paste("Current players:", paste(init_player_list()()$uname, collapse = ", "))),
          actionButton("game_start", "Start game!")
        )
      )
      return(host_waiting_div)
    }
    if(login_status()=="join_waiting"){
      print(paste("For", input$uname, ", game_status()() is", game_status()()))
      # Check whether game_status()() is "setup" or "players_joining"
      # Needs to observe in real time because at some point input$game_start
      # will convert it to "setup" for everyone, at which point we want to
      # skip this div and return the gameboard
      if(game_status()()=="players_joining"){
        print("Returning join_waiting_div")
        join_waiting_div <- div(
          class = "center-both",
          wellPanel(
            h3("Waiting for host to start game"),
            h3(paste("Game ID:", input$game_id)),
            h3(paste("Current players:", paste(init_player_list()()$uname, collapse = ", ")))
          )
        )
        return(join_waiting_div)
      }
    }
    
    div(
      class = "center-both",
      wellPanel(
        h3("Gameboard placeholder!")
      )
    )
  })

  observeEvent(input$new_game_button, {
    print("New game requested")
    login_status("make_new_game")
  })
  observeEvent(input$host_ready_button, {
    print("Host is ready")
    # When "Start hosting!" button is clicked:
    # Create the game files
    # Set login_status to "host_waiting"
    # Redirect to page with current players
    
    if(input$game_id%in%readRDS("game_files/existing_game_ids.rds")){
      login_status("game_id_taken")
    } else {
      login_status("host_waiting")
    }
    
    # Ensure new game IDs are saved AFTER checking whether ID is taken :p
    existing_game_ids <- readRDS("game_files/existing_game_ids.rds")
    saveRDS(c(existing_game_ids, input$game_id), "game_files/existing_game_ids.rds")
    dir.create(paste0("game_files/", input$game_id))
    setGameData("game_status", "players_joining")
    setGameData("init_player_list", data.frame(uname=input$uname, pwd=input$pwd))
    
    init_player_list(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/init_player_list.rds"), 
      readFunc = readRDS
    ))
  })
  observeEvent(input$join_game_button, {
    login_status("join_existing_game")
  })
  observeEvent(input$attempt_join_button, {
    print(paste("Attempting to join game ID", input$game_id))
    if(!input$game_id%in%readRDS("game_files/existing_game_ids.rds")){
      login_status("game_id_not_found")
    } else {
      login_status("found_game_id")
    }
  })
  observeEvent(input$provide_login_info, {
    # Perform a single static check for game_status to see whether the game
    # has been started or not
    # If it HAS NOT been started (e.g. input$game_start says "players_joining" 
    # because input$game_start hasn't been pressed yet)
    # then we want to set the login status to join_waiting
    # then we want to set the game_status object to observing for changes so that
    # when it switches from "players_joining" to "setup" we see it here
    # If the game HAS been started then we want to test for membership in the
    # player_table and compare password
    if(getGameData("game_status")=="players_joining"){
      player_table_static <- getGameData("init_player_list")
      new_player_table_static <- rbind(player_table_static, c("uname"=input$uname, "pwd"=input$pwd))
      setGameData("init_player_list", new_player_table_static)
      login_status("join_waiting")
      
      # Initialize observer so that we can see updates to the player list while waiting
      init_player_list(reactiveFileReader(
        intervalMillis = 100, 
        session = session, 
        filePath = paste0("game_files/", input$game_id, "/init_player_list.rds"), 
        readFunc = readRDS
      ))
      # Initialize observer so that we can see when game_status.rds changes to "setup"
      # This observation doesn't happen here but instead above in XXXX section
      game_status(reactiveFileReader(
        intervalMillis = 100, 
        session = session, 
        filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
        readFunc = readRDS
      ))
    } else {
      # Single static read of final_player_table here is ok because we're being 
      # triggered by button click not by any action from other clients

      # Do still need to initialize a reactive game_status for returning players
      # Don't need to initialize a reactive init_player_list 
      # because we never observe the player list changing if logging in this way
      final_player_table_static <- getGameData("final_player_table")
      
      if(input$uname%in%final_player_table_static$uname){
        player_row <- which(final_player_table_static$uname==input$uname)
        if(input$pwd==final_player_table_static$pwd[player_row]){
          login_status("success")
          game_status(reactiveFileReader(
            intervalMillis = 100, 
            session = session, 
            filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
            readFunc = readRDS
          ))
        } else {
          login_status("join_game_failed")
        }
      } else {
        login_status("join_game_failed")
      }
    }
  })
  observeEvent(input$game_start, {
    # When "Start game!" button is clicked:
    # Create player_table and write to RDS
    # Decide who goes first and write out player_table in order
    # Set login status to "success" so all login_status steps get skipped
    # Set game_status to "setup"
    print("Starting game!")
    login_status("success")
    setGameData("game_status", "setup")
    setGameData("final_player_table", init_player_list()())
    game_status(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
      readFunc = readRDS
    ))
  })
}


browseURL("http://127.0.0.1:5013/")
shinyApp(ui, server, options = list(launch.browser=TRUE, port=5013))