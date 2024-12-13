
library(shiny)
library(plotly)
source("scripts/resource_creation.R")
# options(browser=r"(C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe)")

## To-do:
# Allow players to choose their color
# --Offer color selection somehow
# --Render in the "waiting for players" screens with inline HTML formatting
# --so the names are the actual colors the players are choosing
# --Store in final_player_table (aka init_player_list()())
# Add robber action as a little UFO over the hex
# --Should disable the hex from producing resources
# --Will need to be constructed from polygons (all 7-sided!)

ui <- fillPage(
  uiOutput("visible_screen"),
  includeCSS("styles.css")
)

server <- function(input, output, session){
  getGameData <- function(rds_obj_name, print_value=TRUE){
    # print(paste("Reading", rds_obj_name, "in from file"))
    value <- readRDS(paste0("game_files/", input$game_id, "/", rds_obj_name, ".rds"))
    # if(print_value)print(paste("Value:", value))
    return(value)
  }
  setGameData <- function(rds_obj_name, value, print_value=TRUE){
    # print(paste("Saving", rds_obj_name, "into file"))
    saveRDS(value, paste0("game_files/", input$game_id, "/", rds_obj_name, ".rds"))
    # if(print_value)print(paste("Value:", value))
    return(NULL)
  }
  
  # These vars are simple reactives because they're specific to this session
  login_status <- reactiveVal("startup")
  point_selected <- reactiveVal(FALSE)
  my_build_list <- reactiveVal(data.frame(id=numeric(), owner=character(), build=character()))
  # These vars are fancy reactives because they're shared across sessions
  # All are converted to reactiveFileReaders once we have input$game_id
  init_player_list <- reactiveVal(function(){})
  game_status <- reactiveVal(function(){})
  current_player <- reactiveVal(function(){})
  build_list <- reactiveVal(function(){})

  output$visible_screen <- renderUI({
    print("Rendering visible screen!")
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
          textInput("game_id", label = "Choose a game ID:", value = "QUNWYD"),
          # textInput("game_id", label = "Choose a game ID:", value = suggested_game_id),
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
          textInput("game_id", label = "Choose a game ID:", value = "QUNWYD"), # Remove value after debugging
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
          textInput("uname", label = "Username:", value = "newplayer"), #change back to newplayer eventually
          textInput("pwd", label = "Password:", value = "password"), #change back to alsopassword eventually
          actionButton("provide_login_info", "Join game")
        )
      )
      return(found_game_id_div)
    }
    if(login_status()=="uname_already_taken"){
      print("Returning uname_already_taken_div")
      uname_already_taken_div <- div(
        class = "center-both",
        wellPanel(
          h3("Provide a username and password"),
          textInput("uname", label = "Username:", value = "admin"), #remove value eventually
          p("That username is already taken! Choose a different one.", style="color:red;"),
          textInput("pwd", label = "Password:", value = "password"), #remove value eventually
          actionButton("provide_login_info", "Join game")
        )
      )
      return(uname_already_taken_div)
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
    
    # Load current_player()() here to avoid loading it separately for host and join
    current_player(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/current_player.rds"), 
      readFunc = readRDS
    ))
    build_list(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/build_list.rds"), 
      readFunc = readRDS
    ))
    
    # At this point we switch to using game_status()() instead of login_status()
    # to manage state because login_status() should be "success" for everyone
    # Initial game_status()() is "setup"
    print(paste("First player up is:", current_player()()))
    if(game_status()()=="setup"){
      if(current_player()()==input$uname){
        print("Returning world div with clickables")
        world_div_clickable <- tagList(
          sidebarPanel(
            h3(paste0("Welcome to Planetan, ", input$uname, "!")),
            h3("Choose a starting location by clicking on the globe."),
            actionButton("build_here", label = "Build here?", disabled = TRUE),
            tableOutput("build_info")
          ),
          mainPanel(
            plotlyOutput("game_world", height = "100vh")
          )
        )
        return(world_div_clickable)
      } else {
        print("Returning world div with no clickable points")
        world_div_unclickable <- tagList(
          sidebarPanel(
            h3(paste0("Welcome to Planetan, ", input$uname, "!")),
            h3(paste("Waiting on", current_player()(), "to choose starting location")),
            tableOutput("build_info")
          ),
          mainPanel(
            plotlyOutput("game_world_static", height = "100vh")
          )
        )
        return(world_div_unclickable)
      }
    }
    div(
      class = "center-both",
      wellPanel(
        h3("Gameboard placeholder!")
      )
    )
  })
  output$build_info <- renderTable({
    print("Rendering build_info as a table!")
    build_list()()
  })
  
  observeEvent(input$new_game_button, {
    print("input$new_game_button clicked")
    login_status("make_new_game")
  })
  observeEvent(input$host_ready_button, {
    print("input$host_ready_button clicked")
    # When "Start hosting!" button is clicked:
    # Create the game files
    # Set login_status to "host_waiting"
    # Redirect to page with current players
    
    # Ensure new game IDs are saved AFTER checking whether ID is taken :p
    if(input$game_id%in%readRDS("game_files/existing_game_ids.rds")){
      login_status("game_id_taken")
      return(NULL)
    }
    login_status("host_waiting")

    existing_game_ids <- readRDS("game_files/existing_game_ids.rds")
    saveRDS(c(existing_game_ids, input$game_id), "game_files/existing_game_ids.rds")
    dir.create(paste0("game_files/", input$game_id))
    setGameData("game_status", "players_joining")
    setGameData("init_player_list", data.frame(uname=input$uname, pwd=input$pwd))
    setGameData("build_list", data.frame(id=numeric(), owner=character(), build=character()))
    
    # resource_layout <- getRandomGlobeLayout()
    # built_world <- worldbuilder(resource_layout)
    resource_layout <- readRDS("debug_resource_layout.rds")
    built_world <- readRDS("debug_built_world.rds")
    setGameData("resource_layout", resource_layout, print_value = FALSE)
    setGameData("globe_plates", built_world, print_value = FALSE)
    
    init_player_list(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/init_player_list.rds"), 
      readFunc = readRDS
    ))
  })
  observeEvent(input$join_game_button, {
    print("input$join_game_button clicked")
    login_status("join_existing_game")
  })
  observeEvent(input$attempt_join_button, {
    print("input$attempt_join_button clicked")
    print(paste("Attempting to join game ID", input$game_id))
    if(!input$game_id%in%readRDS("game_files/existing_game_ids.rds")){
      login_status("game_id_not_found")
    } else {
      login_status("found_game_id")
    }
  })
  observeEvent(input$provide_login_info, {
    print("input$provide_login_info clicked")
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
      if(input$uname%in%player_table_static$uname){
        login_status("uname_already_taken")
      } else {
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
        # This observation doesn't happen here but instead above in input$game_start section
        game_status(reactiveFileReader(
          intervalMillis = 100, 
          session = session, 
          filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
          readFunc = readRDS
        ))
      }
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
          # Initialize observer so that we can see when game_status.rds changes to "setup"
          # This observation doesn't happen here but instead below in input$game_start section
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
    print("input$game_start clicked")
    # When "Start game!" button is clicked:
    # Create player_table and write to RDS
    # Decide who goes first and write out player_table in order
    # Set login status to "success" so all login_status steps get skipped
    # Set game_status to "setup"
    print("Starting game!")
    login_status("success")
    setGameData("final_player_table", init_player_list()())
    setGameData("current_player", sample(init_player_list()()$uname, 1))
    setGameData("marker_data_all", marker_data_all, print_value = FALSE)
    setGameData("marker_data_unmoved", marker_data_unmoved, print_value = FALSE)
    setGameData("nearby_structures", nearby_structures, print_value = FALSE)
    setGameData("game_status", "setup")
    game_status(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
      readFunc = readRDS
    ))
  })
  observeEvent(input$build_here, {
    print("input$build_here clicked")
    print("Value of ed():")
    print(ed())
    marker_data_unmoved <- getGameData("marker_data_unmoved", print_value = FALSE)
    build_spot <- marker_data_unmoved[ed()$key,]
    print(build_spot)
    
    if(build_spot$lab=="edge"){
      build_type <- "road"
    } else { #if(build_spot$lab=="vertex")
      if(build_spot$id%in%build_list()()$id){ # if it's been built before
        build_type <- "city"
      } else {
        build_type <- "settlement"
      }
    }
    new_build <- data.frame(id=build_spot$id, owner=input$uname, build=build_type)
    setGameData("build_list", rbind(build_list()(), new_build))
  })
  observeEvent(build_list()(), {
    print(paste("build_list()() triggered for", input$uname))
    anti_merge <- function(x, y, by) {
      x[!do.call(paste, x[by]) %in% do.call(paste, y[by]), ]
    }
    new_build_data <- anti_merge(build_list()(), my_build_list(), by=c("id", "build"))
    if(nrow(new_build_data)==0){
      return(NULL)
    }
    new_build_row_data <- merge(new_build_data[,c("id", "build"),drop=FALSE], marker_data_unmoved)
    new_build_row_list <- split(new_build_row_data, seq_len(nrow(new_build_row_data)))
    new_geoms <- mapply(piece_maker, new_build_data$build, new_build_row_list, SIMPLIFY = FALSE)
    new_geom_combined <- combine_geoms(new_geoms)
    
    nvert_piece_vals <- data.frame(build=c("city", "settlement", "road"), nvert=c(80, 20, 10))
    mesh_offset <- sum(merge(my_build_list(), nvert_piece_vals, all.y = FALSE)$nvert) + nvert_globe_plates

    newtrace <- list(
      x=list(as.list(new_geom_combined$vertices$x)),
      y=list(as.list(new_geom_combined$vertices$y)),
      z=list(as.list(new_geom_combined$vertices$z)),
      i=list(as.list(new_geom_combined$faces$i+mesh_offset)),
      j=list(as.list(new_geom_combined$faces$j+mesh_offset)),
      k=list(as.list(new_geom_combined$faces$k+mesh_offset)),
      facecolor=list(as.list(rep("red", nrow(new_geom_combined$faces))))
    )
    plotlyProxy("game_world") %>% plotlyProxyInvoke("extendTraces", newtrace, list(0))
    plotlyProxy("game_world_static") %>% plotlyProxyInvoke("extendTraces", newtrace, list(0))
    
    # print(my_build_list())
    # print(build_list()())
    my_build_list(build_list()())
  })
  
  output$game_world <- renderPlotly({
    print("Re-rendering game_world")
    globe_plates <- getGameData("globe_plates", print_value = FALSE)
    # Non-local works great, at least for simple variables like this.
    nvert_globe_plates <<- nrow(globe_plates$vertices)
    
    set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
                     autorange=FALSE, showspikes=FALSE,
                     showgrid=FALSE, zeroline=FALSE, visible=FALSE)
    # Maybe build ply() as a reactive object (static object?)
    # at the same time that the world itself is built?
    ply <- plot_ly(source = "game_world") %>%
      add_trace(type="mesh3d", data = globe_plates,
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none") %>%
      layout(scene=list(
        xaxis=set_axis, yaxis=set_axis, zaxis=set_axis,
        aspectmode='cube',
        camera=list(eye=list(x=0.8, y=0.8, z=0.8)),
        bgcolor="black"
      ),
      margin=list(l=0, r=0, b=0, t=0, pad=0),
      showlegend=FALSE) %>%
      config(displayModeBar = FALSE)
    
    # This feels like it should be added with an updatePlotlyProxy, not here...
    # I like the elegance of game_world only really being the globe itself
    # and then the traces are added/removed later
    # Feeling pretty good about visuals/meshes being added via updatePlotly
    # all into trace 0 because those shouldn't need to be removed later
    # and that makes keeping track of the trace numbers a lot easier.
    marker_data_all <- getGameData("marker_data_all", print_value = FALSE)
    ### THIS NEEDS TO BE FIXED LATER
    settlement_spots <- marker_data_all[marker_data_all$lab=="vertex",]
    
    ply <- ply %>%
      add_trace(type="scatter3d", mode="markers", data = settlement_spots,
                x=~x, y=~y, z=~z, key=~id,
                marker=list(color="white", opacity=0.001, size=50))
                # apparently opacity=0 doesn't work lmao and returns 1
    return(ply)
  })
  ed <- reactive(event_data(event = "plotly_click", source = "game_world"))
  observeEvent(ed(), {
    req(ed()$key) # Prevent clicks on the GLOBE (not markers) from registering
    print("game_world clicked!")
    print(ed())
    # Safe to delete trace 2 if it doesn't exist
    # I need some way of managing what traces have been added and in what order
    # Trace stack will consist of intermixed markers and meshes
    # Meshes will accumulate over time (or max out at two of them?) but not be removed
    # Markers will get added and removed
    # trace_stack <- data.frame(name=c("globe"), trace_id=0)
    
    # Initially, trace 0 is the globe and trace 1 is the markers added afterward
    
    plotlyProxy("game_world") %>% plotlyProxyInvoke("deleteTraces", 2) # Remove colored trace if it exists
    newtrace <- list(x = list(ed()$x), y = list(ed()$y), z=list(ed()$z), type = "scatter3d",
                     mode = "markers", marker=list(color="red", opacity=0.2, size=50))
    plotlyProxy("game_world") %>% plotlyProxyInvoke("addTraces", newtrace)
    
    updateActionButton(session, "build_here", label = "Build here?", disabled = FALSE)
  })
  output$game_world_static <- renderPlotly({
    print("Re-rendering static game_world")
    globe_plates <- getGameData("globe_plates", print_value = FALSE)
    set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
                     autorange=FALSE, showspikes=FALSE,
                     showgrid=FALSE, zeroline=FALSE, visible=FALSE)
    plot_ly(source="static_globe") %>% # Source shouldn't be used
      add_trace(type="mesh3d", data = globe_plates,
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none") %>%
      layout(scene=list(
        xaxis=set_axis, yaxis=set_axis, zaxis=set_axis,
        aspectmode='cube',
        camera=list(eye=list(x=0.8, y=0.8, z=0.8)),
        bgcolor="black"
      ),
      margin=list(l=0, r=0, b=0, t=0, pad=0),
      showlegend=FALSE) %>%
      config(displayModeBar = FALSE)
  })
}


# if(dir.exists("game_files"))unlink("game_files", recursive = TRUE)
# if(!dir.exists("game_files"))dir.create("game_files")
# if(!file.exists("game_files/existing_game_ids.rds")){
#   saveRDS("ABC", "game_files/existing_game_ids.rds")
# }
browseURL("http://127.0.0.1:5013/")
shinyApp(ui, server, options = list(launch.browser=TRUE, port=5013))