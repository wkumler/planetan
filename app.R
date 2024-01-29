
# Setup ----
library(shiny)
library(plotly)
source("scripts/resource_creation.R")
options(browser=r"(C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe)")

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
  build_list <- reactiveVal(NULL)
  my_uname <- reactive({
    if(is.null(input$host_uname)){
      input$join_uname
    } else {
      input$host_uname
    }
  })

  
  game_id <- paste(sample(LETTERS, 10), collapse = "")
  game_id <- "ABC"
  game_dir <- paste0("game_files/", game_id, "/")
  dir.create(game_dir)
  saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  saveRDS(1, file = paste0(game_dir, "current_player_idx.rds"))
  
  out_of_date <- reactiveFileReader(100, session, paste0(game_dir, "out_of_date.rds"), readFunc = readRDS)
  observeEvent(out_of_date(), {
    player_table(readRDS(paste0(game_dir, "player_table.rds")))
    # build_list(readRDS(paste0(game_dir, "build_list.rds")))
    game_begun(readRDS(paste0(game_dir, "game_begun.rds")))
    current_player_idx(readRDS(paste0(game_dir, "current_player_idx.rds")))
    
    if(game_begun())join_wait(FALSE)
  }, ignoreInit = TRUE)
  
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
    
    # Generate random world
    globe_layout <- getRandomGlobeLayout()
    saveRDS(globe_layout, paste0(game_dir, "globe_layout.rds"))
    saveRDS(worldbuilder(globe_layout), paste0(game_dir, "globe_plates.rds"))
    
    host_wait(TRUE)
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  }, ignoreInit = TRUE)
  observeEvent(input$join_ready_button, {
    join_start(FALSE)
    game_id <- input$game_id_entered
    game_dir <- paste0("game_files/", game_id, "/")
    
    attempted_to_join_nonexistent_game(FALSE)
    failed_login(FALSE)

    if(!dir.exists(game_dir)){
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
        
        join_wait(TRUE)
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$game_start, {
    host_wait(FALSE)
    join_wait(FALSE)
    game_begun(TRUE)
    
    player_table(player_table()[sample(1:nrow(player_table())),])
    build_list(data.frame(id=numeric(), owner=character()))
    
    saveRDS(player_table(), paste0(game_dir, "player_table.rds"))
    saveRDS(build_list(), paste0(game_dir, "build_list.rds"))
    saveRDS(TRUE, paste0(game_dir, "game_begun.rds"))
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  }, ignoreInit = TRUE)
  
  choose_start_spots <- reactiveVal(TRUE)
  setup_settle_placed <- reactiveVal(FALSE)
  setup_road_placed <- reactiveVal(TRUE)
  current_player_idx <- reactiveVal(1)
  output$setup_world <- renderPlotly({
    globe_layout <- readRDS(paste0(game_dir, "globe_layout.rds"))
    globe_plates <- readRDS(paste0(game_dir, "globe_plates.rds"))
    set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
                     autorange=FALSE, showspikes=FALSE,
                     showgrid=FALSE, zeroline=FALSE, visible=FALSE)
    ply <- plot_ly(source = "setup") %>%
      add_trace(type="mesh3d", data = globe_plates,
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none")
    
    my_turn <- player_table()$uname[current_player_idx()]==my_uname()
    if(choose_start_spots() & my_turn){
      marker_data_labeled <- marker_data_all[marker_data_all$lab=="settlement",]
      ply <- ply %>%
        add_trace(type="scatter3d", mode="markers", 
                  data = marker_data_labeled,
                  x=~x, y=~y, z=~z, key=~id, text=~lab,
                  marker=list(
                    color="white", opacity=0.1, size=50
                  ),
                  hoverinfo="text",
                  hovertemplate=paste0("Build a %{text}?<extra></extra>"))
    }
    ply
  })
  
  ed <- reactive(suppressWarnings(event_data(event = "plotly_click", source = "setup")))
  observeEvent(ed(), {
    req(ed()$key)
    print("Noticed click!")
    
    clicked_point_id <- as.numeric(ed()$key)
    clicked_point_data <- marker_data_unmoved[clicked_point_id, ]
    piece_to_build <- clicked_point_data$lab
    print(piece_to_build)
    
    current_uname <- player_table()$uname[current_player_idx()]
    build_list(rbind(build_list(), data.frame(clicked_point_id, current_uname)))
    print(build_list())
    saveRDS(build_list(), paste0(game_dir, "build_list.rds"))
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  })
  
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
      if(choose_start_spots()){
        my_turn <- player_table()$uname[current_player_idx()]==my_uname()
        drawSetupSpots(my_uname=my_uname(), my_turn=my_turn)
      } else if(my_turn()){
        drawInteractiveBoard()
      } else {
        drawDawdleBoard()
      }
    } else {
      div()
    }
  })
}

# sub-functions ----
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
drawSetupSpots <- function(my_uname, my_turn){
  welcome_message <- if(my_turn){
    h3("Choose a starting settlement by clicking on the globe")
  } else {
    h3("Waiting for other players to choose starting spots...")
  }
  tagList(
    sidebarPanel(
      h3(paste0("Welcome to Planetan, ", my_uname, "!")),
      welcome_message
    ),
    mainPanel(
      plotlyOutput("setup_world", height = "100vh")
    )
  )
}

# Run app ----
# browseURL("127.0.0.1:5013")
shinyApp(ui, server, options = list(launch.browser=TRUE, port=5013))