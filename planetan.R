
library(shiny)
library(plotly)
source("scripts/resource_creation.R")
anti_merge <- function(x, y, by) {
  x[!do.call(paste, x[by]) %in% do.call(paste, y[by]), ]
}

# options(browser=r"(C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe)")

## To-do:
# Allow players to choose their color
# --Offer color selection somehow
# --Render in the "waiting for players" screens with inline HTML formatting
# --so the names are the actual colors the players are choosing

ui <- fillPage(
  uiOutput("visible_screen"),
  includeCSS("styles.css")
)

server <- function(input, output, session){
  getGameData <- function(rds_obj_name, print_value=TRUE){
    print(paste("Reading", rds_obj_name, "in from file"))
    value <- readRDS(paste0("game_files/", input$game_id, "/", rds_obj_name, ".rds"))
    # if(print_value)print(paste("Value:", value))
    return(value)
  }
  setGameData <- function(rds_obj_name, value, print_value=TRUE){
    print(paste("Saving", rds_obj_name, "into file"))
    saveRDS(value, paste0("game_files/", input$game_id, "/", rds_obj_name, ".rds"))
    # if(print_value)print(paste("Value:", value))
    return(NULL)
  }
  
  # These vars are simple reactives because they're specific to this session
  login_status <- reactiveVal("startup")
  my_build_list <- reactiveVal(data.frame(id=numeric(), owner=character(), build=character()))
  my_marker_data <- reactiveVal(data.frame(id=numeric(), x=numeric(), y=numeric(), z=numeric(),
                                           compass_angle=numeric(), elevation_angle=numeric(),
                                           lab=character()))
  # These vars are fancy reactives because they're shared ACROSS sessions
  # All are converted to reactiveFileReaders once we have input$game_id
  init_player_list <- reactiveVal(function(){})
  game_status <- reactiveVal(function(){})
  current_player <- reactiveVal(function(){})
  build_list <- reactiveVal(function(){})
  marker_data <- reactiveVal(function(){})

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
      if(game_status()()=="players_joining"){
        print("Returning found_newgame_id_div")
        found_newgame_id_div <- div(
          class = "center-both",
          wellPanel(
            h3("Provide a username and password"),
            textInput("uname", label = "Username:", value = "newplayer"), #change back to newplayer eventually
            textInput("pwd", label = "Password:", value = "password"), #change back to alsopassword eventually
            actionButton("join_waiting_room", "Join game")
          )
        )
        return(found_newgame_id_div)
      } else {
        print("Returning found_game_id_div")
        found_game_id_div <- div(
          class = "center-both",
          wellPanel(
            h3("Provide your username and password"),
            textInput("uname", label = "Username:", value = "newplayer"), #change back to newplayer eventually
            textInput("pwd", label = "Password:", value = "password"), #change back to alsopassword eventually
            actionButton("rejoin_existing_game", "Join game")
          )
        )
        return(found_game_id_div)
      }
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
          actionButton("rejoin_existing_game", "Join game")
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
          actionButton("join_waiting_room", "Join game"),
          actionButton("new_game_button", "Host new game")
        )
      )
      return(join_game_failed_div)
    }
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
    marker_data(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/marker_data.rds"), 
      readFunc = readRDS
    ))

    print(paste("First player up is:", current_player()()))
    if(game_status()()=="setup"){
      print("Returning world div")
      if(input$uname==current_player()()){
        world_div <- tagList(
          sidebarPanel(
            h3(paste0("Welcome to Planetan, ", input$uname, "!")),
            h3("Choose a starting location by clicking on the globe."),
            actionButton("build_here_setup", label = "Build here?", disabled = TRUE),
            tableOutput("build_info")
          ),
          mainPanel(
            plotlyOutput("game_world", height = "100vh")
          )
        )
      } else {
        world_div <- tagList(
          sidebarPanel(
            h3(paste0("Welcome to Planetan, ", input$uname, "!")),
            h3(paste0("Waiting for ", current_player()(), " to choose setup spots.")),
            tableOutput("build_info")
          ),
          mainPanel(
            plotlyOutput("game_world", height = "100vh")
          )
        )
      }
      return(world_div)
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
    game_status(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
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
      # Initialize observer so that we can see when game_status.rds changes to "setup"
      # This observation doesn't happen here but instead above in input$game_start section
      game_status(reactiveFileReader(
        intervalMillis = 100, 
        session = session, 
        filePath = paste0("game_files/", input$game_id, "/game_status.rds"), 
        readFunc = readRDS
      ))
    }
  })
  observeEvent(input$join_waiting_room, {
    print("input$join_waiting_room clicked")
    init_player_list(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/init_player_list.rds"), 
      readFunc = readRDS
    ))
    if(input$uname%in%init_player_list()()$uname){
      login_status("uname_already_taken")
    } else {
      new_player_table <- rbind(init_player_list()(), c("uname"=input$uname, "pwd"=input$pwd))
      setGameData("init_player_list", new_player_table)
      login_status("join_waiting")
    }
  })
  observeEvent(input$rejoin_existing_game, {
    init_player_list(reactiveFileReader(
      intervalMillis = 100, 
      session = session, 
      filePath = paste0("game_files/", input$game_id, "/init_player_list.rds"), 
      readFunc = readRDS
    ))
    if(!input$uname%in%init_player_list()()$uname){
      login_status("join_game_failed")
      return(NULL)
    }
    player_row <- which(init_player_list()()$uname==input$uname)
    if(input$pwd!=init_player_list()()$pwd[player_row]){
      login_status("join_game_failed")
      return(NULL)
    }
    login_status("success")
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
    new_play_order <- init_player_list()()[sample(1:nrow(init_player_list()())),]
    setGameData("init_player_list", new_play_order)
    setGameData("current_player", new_play_order$uname[1])
    setGameData("marker_data_all", marker_data_all, print_value = FALSE)
    setGameData("marker_data_unmoved", marker_data_unmoved, print_value = FALSE)
    setGameData("nearby_structures", nearby_structures, print_value = FALSE)
    setGameData("build_list", data.frame(id=numeric(), owner=character(), build=character()))
    setGameData("marker_data", data.frame(id=numeric(), x=numeric(), y=numeric(), z=numeric(),
                                          compass_angle=numeric(), elevation_angle=numeric(),
                                          lab=character()))
    setGameData("game_status", "setup")
  })
  observeEvent(input$build_here_setup, {
    print("input$build_here_setup clicked")
    print(ed())

    print("Updating build_list()()")
    marker_data_unmoved <- getGameData("marker_data_unmoved", print_value = FALSE)
    build_spot <- marker_data_unmoved[ed()$key,]
    build_type <- ifelse(build_spot$lab=="edge", "road", "settlement")
    new_build <- data.frame(id=build_spot$id, owner=input$uname, build=build_type)
    setGameData("build_list", rbind(build_list()(), new_build))
    
    if(new_build$build=="road"){
      print("Updating marker_data()()")
      init_settlement_spots <- marker_data_all[marker_data_all$lab=="vertex",]
      built_verts <- init_settlement_spots[build_list()()$id,]
      too_close_verts <- unique(unlist(merge(built_verts, nearby_structures$verts)$nearest_verts))
      marker_spots <- init_settlement_spots[!init_settlement_spots$id%in%too_close_verts,]
      marker_spots <- marker_spots[!marker_spots$id%in%built_verts$id,]
    } else {
      init_settlement_spots <- marker_data_all[marker_data_all$lab=="edge",]
      nearby_edges <- unlist(nearby_structures$verts$nearest_edges[new_build$id])
      marker_spots <- init_settlement_spots[init_settlement_spots$id%in%nearby_edges,]
    }
    print("Updating marker_data()()")
    setGameData("marker_data", marker_spots)
    
    setup_stack <- init_player_list()()$uname
    setup_stack <- c(setup_stack, rev(setup_stack), "begin")
    next_player <- setup_stack[floor(nrow(getGameData("build_list"))/2)+1]
    if(next_player=="begin"){
      setGameData("game_status", "gameplay")
      setGameData("current_player", setup_stack[1])
      return(NULL)
    } 
    if(new_build$build=="road"){
      setGameData("current_player", next_player)
    }
  })
  observeEvent(build_list()(), {
    print(paste("build_list()() triggered for", input$uname))

    new_build_data <- anti_merge(build_list()(), my_build_list(), by=c("id", "build"))
    if(nrow(new_build_data)==0){
      return(NULL)
    }
    new_build_row_data <- merge(new_build_data[,c("id", "build"),drop=FALSE], marker_data_unmoved)
    new_build_row_list <- split(new_build_row_data, seq_len(nrow(new_build_row_data)))
    new_geoms <- mapply(piece_maker, new_build_row_data$build, new_build_row_list, SIMPLIFY = FALSE)
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
    
    print("Updating my_build_list to match disk")
    my_build_list(build_list()())
  })
  observeEvent(marker_data()(), {
    print(paste("marker_data()() triggered for", input$uname))
    new_marker_data <- anti_merge(marker_data()(), my_marker_data(), by=c("id", "x", "y", "z", "lab"))
    if(nrow(new_marker_data)==0){
      return(NULL)
    }
    
    print("Wiping existing clickables")
    plotlyProxy("game_world") %>% plotlyProxyInvoke("deleteTraces", list(3))
    plotlyProxy("game_world") %>% plotlyProxyInvoke("deleteTraces", list(2))
    
    if(input$uname==getGameData("current_player")){
      print(paste("Adding clickables to map for", input$uname))
      newtrace <- list(
        x = as.list(new_marker_data$x),
        y = as.list(new_marker_data$y),
        z = as.list(new_marker_data$z),
        key = as.list(new_marker_data$id),
        type = "scatter3d",
        mode = "markers",
        marker = list(color="white", opacity=0.1, size=50)
      )
      plotlyProxy("game_world") %>% plotlyProxyInvoke("addTraces", newtrace)
    } else {
      print(paste("No markers to add for", input$uname))
    }
    print("Updating my_marker_data to match disk")
    my_marker_data(marker_data()())
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
    
    center_spot <- data.frame(x=0, y=0, z=0, compass_angle=0, elevation_angle=0)
    robber_init <- piece_maker(piece_type = "robber", center_spot)
    ply <- ply %>%
      add_trace(type="mesh3d", data = robber_init,
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(robber_init$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none")
    
    if(isolate(current_player()())==input$uname){
      ply <- ply %>%
        add_trace(type="scatter3d", data=marker_data_all[marker_data_all$lab=="vertex",],
                  x=~x, y=~y, z=~z, key=~id, mode="markers",
                  marker=list(color="white", opacity=0.1, size=50))
    }
    
    # Trace 0 = globe
    # Trace 1 = robber
    # Trace 2 = all markers
    # Trace 3 = selected spot: ed()
    return(ply)
  })
  ed <- reactive(event_data(event = "plotly_click", source = "game_world"))
  observeEvent(ed(), {
    req(ed()$key) # Prevent clicks on the GLOBE (not markers) from registering
    print("game_world clicked!")
    print(ed())
    newtrace <- list(x = list(ed()$x), y = list(ed()$y), z=list(ed()$z), key=list(ed()$key), type = "scatter3d",
                     mode = "markers", marker=list(color="red", opacity=0.2, size=50))
    plotlyProxy("game_world") %>% plotlyProxyInvoke("deleteTraces", list(3)) # Remove colored trace if it exists
    plotlyProxy("game_world") %>% plotlyProxyInvoke("addTraces", newtrace)
    
    updateActionButton(session, "build_here_setup", label = "Build here?", disabled = FALSE)
  })
}


if(dir.exists("game_files"))unlink("game_files", recursive = TRUE)
if(!dir.exists("game_files"))dir.create("game_files")
if(!file.exists("game_files/existing_game_ids.rds")){
  saveRDS("ABC", "game_files/existing_game_ids.rds")
}
browseURL("http://127.0.0.1:5013/")
shinyApp(ui, server, options = list(launch.browser=TRUE, port=5013))