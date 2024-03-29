
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
  player_resources <- reactiveVal(NULL)
  build_list <- reactiveVal(NULL)
  built_pieces <- reactiveVal(NULL)
  my_uname <- reactive({
    if(is.null(input$host_uname)){
      input$join_uname
    } else {
      input$host_uname
    }
  })
  
  choose_start_spots <- reactiveVal(TRUE)
  placing_setup_settlement <- reactiveVal(TRUE)
  
  game_id <- paste(sample(LETTERS, 10), collapse = "")
  game_id <- "ABC"
  game_dir <- paste0("game_files/", game_id, "/")
  dir.create(game_dir)
  
  saveRDS(1, file = paste0(game_dir, "current_player_idx.rds"))
  saveRDS(TRUE, file = paste0(game_dir, "choose_start_spots.rds"))
  saveRDS(TRUE, file = paste0(game_dir, "player_resources.rds"))
  print("Tripping out-of-date from initial server start")
  saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  
  out_of_date <- reactiveFileReader(100, session, paste0(game_dir, "out_of_date.rds"), readFunc = readRDS)
  observeEvent(out_of_date(), {
    print("Noticed out of date!")
    player_table(readRDS(paste0(game_dir, "player_table.rds")))
    player_resources(readRDS(paste0(game_dir, "player_resources.rds")))
    build_list(readRDS(paste0(game_dir, "build_list.rds")))
    game_begun(readRDS(paste0(game_dir, "game_begun.rds")))
    current_player_idx(readRDS(paste0(game_dir, "current_player_idx.rds")))
    built_pieces(readRDS(paste0(game_dir, "built_pieces.rds")))
    choose_start_spots(readRDS(paste0(game_dir, "choose_start_spots.rds")))
    
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
    
    build_list(data.frame(id=numeric(), owner=character()))
    saveRDS(build_list(), paste0(game_dir, "build_list.rds"))
    
    empty_built_pieces <- list(
      vertices=data.frame(x=numeric(),y=numeric(),z=numeric()), 
      faces=data.frame(i=numeric(),j=numeric(),k=numeric(), color=character())
    )
    saveRDS(empty_built_pieces, paste0(game_dir, "built_pieces.rds"))
    
    
    # Generate random world
    # globe_layout <- getRandomGlobeLayout()
    # saveRDS(globe_layout, paste0(game_dir, "globe_layout.rds"))
    # saveRDS(worldbuilder(globe_layout), paste0(game_dir, "globe_plates.rds"))
    
    
    host_wait(TRUE)
    print("Tripping out-of-date from input$host_ready")
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
        print("Tripping out-of-date from input$join_ready_button")
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
    saveRDS(player_table(), paste0(game_dir, "player_table.rds"))
    
    player_resources(data.frame(uname=player_table()$uname, wood=0, brick=0, wool=0, wheat=0, ore=0))
    saveRDS(player_resources(), paste0(game_dir, "player_resources.rds"))
    
    saveRDS(TRUE, paste0(game_dir, 'placing_setup_settlement.rds'))
    saveRDS(TRUE, paste0(game_dir, "game_begun.rds"))
    saveRDS(FALSE, paste0(game_dir, "choose_setup_spots.rds"))
    
    print("Tripping out-of-date from input$game_start")
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  }, ignoreInit = TRUE)
  
  setup_settle_placed <- reactiveVal(FALSE)
  setup_road_placed <- reactiveVal(FALSE)
  current_player_idx <- reactiveVal(1)
  output$setup_world <- renderPlotly({
    globe_layout <- readRDS("debug_globe_layout.rds")
    globe_plates <- readRDS("debug_globe_plates.rds")
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
                hoverinfo="none") %>%
      add_trace(type="mesh3d", data = built_pieces(),
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none") %>%
      layout(scene=list(
        xaxis=set_axis, yaxis=set_axis, zaxis=set_axis,
        aspectmode='cube',
        camera=list(eye=list(x=0.8, y=0.8,z=0.8)),
        bgcolor="black"
      ),
      margin=list(l=0, r=0, b=0, t=0, pad=0),
      showlegend=FALSE) %>%
      config(displayModeBar = FALSE)
    
    my_turn <- player_table()$uname[current_player_idx()]==my_uname()
    if(choose_start_spots() & my_turn){
      placing_setup_settlement <- readRDS(paste0(game_dir, 'placing_setup_settlement.rds'))
      if(placing_setup_settlement){
        print("Attempting to render settlement markers")
        marker_data_labeled <- marker_data_all[marker_data_all$lab=="settlement",]
        
        nearby_spots <- nearby_structures$verts[nearby_structures$verts$id%in%build_list()$id,]
        illegal_spots <- unique(c(build_list()$id, unlist(nearby_spots)))
        marker_data_labeled <- marker_data_labeled[!marker_data_labeled$id%in%illegal_spots,]
        
        saveRDS(FALSE, paste0(game_dir, 'placing_setup_settlement.rds'))
      } else {
        print("Attempting to render road markers")
        print(build_list())
        last_built_settlement_idx <- nearby_structures$verts$id==tail(build_list(), 1)$id
        setup_nearby_roads <- nearby_structures$verts[last_built_settlement_idx,]$nearest_edges[[1]]
        marker_data_labeled <- marker_data_all[marker_data_all$id%in%setup_nearby_roads,]
        
        saveRDS(TRUE, paste0(game_dir, 'placing_setup_settlement.rds'))
      }
      # print(build_list())
      # print(marker_data_labeled)
      ply <- ply %>%
        add_trace(type="scatter3d", mode="markers", data = marker_data_labeled, 
                  x=~x, y=~y, z=~z, key=~id, text=~lab, hoverinfo="text",
                  marker=list(color="white", opacity=0.1, size=50),
                  hovertemplate=paste0("Build a %{text}?<extra></extra>"))
    }
    ply
  })
  ed_setup <- reactive(event_data(event = "plotly_click", source = "setup"))
  observeEvent(ed_setup(), {
    req(ed_setup()$key)
    print("Noticed click!")
    
    clicked_point_id <- as.numeric(ed_setup()$key)
    clicked_point_data <- marker_data_unmoved[clicked_point_id, ]
    piece_to_build <- clicked_point_data$lab
    print(piece_to_build)
    
    current_uname <- player_table()$uname[current_player_idx()]
    new_build_list <- rbind(build_list(), data.frame(id=clicked_point_id, owner=current_uname))
    saveRDS(new_build_list, paste0(game_dir, "build_list.rds"))
    addPieceToFixed(game_dir, clicked_point_id, current_uname)
    
    if(piece_to_build=="road"){
      builds_per_person <- nrow(new_build_list)/(2*nrow(player_table()))
      print(paste("Builds per person:", builds_per_person))
      if(builds_per_person<1){
        current_player_idx(current_player_idx()+1)
      } else if(builds_per_person==1){
        current_player_idx(current_player_idx())
      } else if(builds_per_person < 2){
        current_player_idx(current_player_idx()-1)
      } else {
        current_player_idx(1)
        saveRDS(FALSE, paste0(game_dir, "choose_start_spots.rds"))
        
        # After all startup spots are placed, allocate initial resources
        print(player_resources())
        p_res <- as.matrix(player_resources()[,2:6])
        rownames(p_res) <- player_resources()$uname
        player_faces <- merge(build_list()[build_list()$id<60,], nearby_structures$verts)
        player_faces <- player_faces[(nrow(player_faces)/2+1):nrow(player_faces),]
        player_faces <- data.frame(owner=rep(player_faces$owner, each=3), 
                                   id=unlist(player_faces$nearest_faces))
        alloc_res <- merge(player_faces, globe_layout)[,c("id", "owner", "hex_resources")]
        for(i in seq_len(nrow(alloc_res))){
          p_res[alloc_res$owner[i], alloc_res$hex_resources[i]] <- 
            p_res[alloc_res$owner[i], alloc_res$hex_resources[i]]+1
        }
        print("Initial resources:")
        print(p_res)
      }
      saveRDS(current_player_idx(), paste0(game_dir, "current_player_idx.rds")) 
    }

    print("Tripping out-of-date from ed_setup()")
    saveRDS(runif(1), file = paste0(game_dir, "out_of_date.rds"))
  })
  
  output$game_world <- renderPlotly({
    globe_layout <- readRDS("debug_globe_layout.rds")
    globe_plates <- readRDS("debug_globe_plates.rds")
    print("Rendering game world")
    set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
                     autorange=FALSE, showspikes=FALSE,
                     showgrid=FALSE, zeroline=FALSE, visible=FALSE)
    ply <- plot_ly(source = "game") %>%
      add_trace(type="mesh3d", data = globe_plates,
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none") %>%
      add_trace(type="mesh3d", data = built_pieces(),
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none") %>%
      layout(scene=list(
        xaxis=set_axis, yaxis=set_axis, zaxis=set_axis,
        aspectmode='cube',
        camera=list(eye=list(x=0.8, y=0.8,z=0.8)),
        bgcolor="black"
      ),
      margin=list(l=0, r=0, b=0, t=0, pad=0),
      showlegend=FALSE) %>%
      config(displayModeBar = FALSE)
    
    cur_uname <- player_table()$uname[current_player_idx()]
    if(cur_uname==my_uname()){
      print(paste("Calculating build spots for player", cur_uname))
      player_built_spots <- build_list()[build_list()$owner==cur_uname,]
      player_struct <- merge(player_built_spots, rbind(nearby_structures$verts, nearby_structures$edges))
      player_struct <- merge(player_struct, marker_data_all)
      nearby_spots_ids <- unlist(player_struct$nearest_edges)
      
      too_close_verts <- merge(build_list(), marker_data_all[,c("id", "lab")])
      too_close_verts <- too_close_verts[too_close_verts$lab!="road",]
      too_close_verts <- unlist(merge(too_close_verts, nearby_structures$verts)$nearest_verts)
      
      
      illegal_spots <- unique(c(build_list()$id, too_close_verts))
      open_spots <- setdiff(nearby_spots_ids, illegal_spots)
      marker_data_labeled <- marker_data_all[marker_data_all$id%in%open_spots,]
      
      ply <- ply %>%
        add_trace(type="scatter3d", mode="markers", data = marker_data_labeled, 
                  x=~x, y=~y, z=~z, key=~id, text=~lab, hoverinfo="text",
                  marker=list(color="white", opacity=0.1, size=50),
                  hovertemplate=paste0("Build a %{text}?<extra></extra>"))
    }
    ply
  })
  ed_game <- reactive(event_data(event = "plotly_click", source = "gameboard"))
  observeEvent(ed_setup(), {
    ### FILL THIS IN NEXT
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
      my_turn <- player_table()$uname[current_player_idx()]==my_uname()
      if(choose_start_spots()){
        drawSetupSpots(my_uname=my_uname(), my_turn=my_turn)
      } else {
        print("Drawing gameboard")
        drawGameboard(my_uname=my_uname(), my_turn=my_turn, player_res=player_resources())
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
drawGameboard <- function(my_uname, my_turn, player_res){
  tagList(
    sidebarPanel(
      h3(paste0("Welcome to Planetan, ", my_uname, "!")),
      h3("Current resources:"),
      renderTable(player_res)
    ),
    mainPanel(
      plotlyOutput("game_world", height = "100vh")
    )
  )
}
addPieceToFixed <- function(game_dir, clicked_point_id, current_uname){
  print("Adding piece to fixed list")
  built_pieces <- readRDS(paste0(game_dir, "built_pieces.rds"))
  piece_to_build <- marker_data_unmoved[marker_data_all$id==clicked_point_id,]
  new_piece <- piece_maker(piece_to_build$lab, piece_to_build)
  built_pieces <- combine_geoms(list(built_pieces, new_piece))
  saveRDS(built_pieces, paste0(game_dir, "built_pieces.rds"))
}


# Run app ----
# browseURL("127.0.0.1:5013")
shinyApp(ui, server, options = list(launch.browser=TRUE, port=5013))