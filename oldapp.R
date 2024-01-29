

# Setup ----
set.seed(123)
library(shiny)
library(plotly)
options(browser=r"(C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe)")


source("scripts/resource_creation.R")
get_markers <- function(build_list, current_player, city_list, player_resources){
  player_builds <- build_list[build_list$owner==current_player,]
  p1_all_data <- merge(player_builds, marker_data_all)
  p1_road_ids <- p1_all_data[p1_all_data$lab=="road","id",drop=FALSE]
  
  player_i_resources <- player_resources[paste0("P", current_player),]
  if(player_i_resources$wood>=1 & player_i_resources$brick>=1){
    possible_roads <- unique(unlist(merge(player_builds, nearby_structures$edges)$nearest_edges))
    possible_roads <- possible_roads[!possible_roads%in%build_list[,"id"]]
    road_markers <- marker_data_all[possible_roads,]
  } else {
    road_markers <- marker_data_all[numeric(0),]
  }
  
  if(player_i_resources$ore>=3 & player_i_resources$wheat>=2){
    possible_cities <- p1_all_data[p1_all_data$lab=="settlement","id",drop=TRUE]
    possible_cities <- possible_cities[!possible_cities%in%city_list]
    city_markers <- marker_data_all[possible_cities,]
  } else {
    city_markers <- marker_data_all[numeric(0),]
  }
  if(nrow(city_markers)>0){
    city_markers[c("x", "y", "z")] <- city_markers[c("x", "y", "z")] %>%
      apply(1, cart2sphere) %>% `+`(c(1, 0, 0)) %>% apply(2, sphere2cart) %>%
      t() %>% setNames(c("x", "y", "z"))
  }
  
  if(player_i_resources$wood>=1 & player_i_resources$brick>=1 & 
     player_i_resources$wheat>=1 & player_i_resources$wool>=1){
    possible_settles <- unlist(merge(p1_road_ids, nearby_structures$edges)$nearest_verts)
    sorted_build_list <- build_list[order(build_list$id),]
    merged_labs <- merge(sorted_build_list, marker_data_all, sort = FALSE)$lab
    built_verts <- sorted_build_list[merged_labs%in%c("city", "settlement"),"id", drop=FALSE]
    forbidden_verts <- unlist(merge(built_verts, nearby_structures$verts)$nearest_verts)
    forbidden_verts <- unique(c(forbidden_verts, built_verts$id))
    possible_settles <- possible_settles[!possible_settles%in%forbidden_verts]
    settle_markers <- marker_data_all[possible_settles,]
  } else {
    settle_markers <- marker_data_all[numeric(0),]
  }
  
  all_markers <- rbind(road_markers, settle_markers, city_markers)
  # dput(all_markers)
  all_markers
}

# Create initial data for interactable markers
marker_data_unmoved  <- rbind(
  cbind(TI_structure$vertices, lab="settlement"),
  cbind(TI_structure$edges, lab="road")
)
# Move them slightly outside the globe to improve interactivity
marker_data_all <- marker_data_unmoved
marker_data_all[c("x", "y", "z")] <- marker_data_all[c("x", "y", "z")] %>%
  apply(1, cart2sphere) %>% `+`(c(0.5, 0, 0)) %>% apply(2, sphere2cart) %>%
  t() %>% setNames(c("x", "y", "z"))
player_colors <- viridis::plasma(4)
saveRDS(numeric(0), "all_player_ids.rds")
saveRDS(1, "current_player.rds")
saveRDS(1, "out_of_date.rds")
saveRDS(numeric(), "citylist_to_write.rds")
saveRDS(numeric(4), "devcardcounts_to_write.rds")

hex_faces <- data.frame(id=TI_structure$faces[,"id",drop=FALSE][2:31,])
chit_values <- rep(c(2:6, 8:12), each=3)
hex_faces$chit_value <- sample(chit_values, 30, replace = FALSE)
hex_faces <- rbind(data.frame(id=151, chit_value=0), hex_faces, data.frame(id=182, chit_value=0))
hex_faces$resource <- hex_resources

build_list <- data.frame(id=c(4, 90, 21, 69), owner=c(1,1,2,2))

init_settle_list <- lapply(which(TI_structure$vertices$id%in%build_list$id), function(row_id){
  piece_maker("settlement", TI_structure$vertices[row_id, ], 
              color=player_colors[build_list$owner[build_list$id==row_id]])
}) %>% combine_geoms()
init_road_list <- lapply(which(TI_structure$edges$id%in%build_list$id), function(row_id){
  piece_maker("road", TI_structure$edges[row_id, ], 
              color=player_colors[build_list$owner[build_list$id==(row_id+60)]])
}) %>% combine_geoms()
init_pieces <- combine_geoms(list(init_settle_list, init_road_list))
saveRDS(init_pieces, "fixed_to_write.rds")

buildlist_to_write <- build_list
saveRDS(buildlist_to_write, file="buildlist_to_write.rds")

resources_to_write <- data.frame(
  matrix(rep(5, 10), nrow = 2, dimnames = list(
    paste0("P", 1:2), c("wood", "brick", "wool", "wheat", "ore")))
)
saveRDS(resources_to_write, file="resources_to_write.rds")
markers_clickable <- get_markers(build_list, 1, numeric(), resources_to_write)


# UI ----
ui <- fillPage(
  sidebarLayout(
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
  ),
  tags$head(
    tags$style(
      HTML("body {background-color: black;}")
    )
  )
)

# Server ----
server <- function(input, output, session){
  ### Multiplayer things ----
  # This section runs once when a new player joins
  player_list <- readRDS("all_player_ids.rds")
  player_id <- length(player_list)+1
  saveRDS(c(player_list, player_id), "all_player_ids.rds")
  
  out_of_date <- reactiveFileReader(100, session, "out_of_date.rds", readFunc = readRDS)
  observeEvent(out_of_date(), {
    current_player(readRDS("current_player.rds"))
    print(paste("Just read current player from file, new value:", current_player()))
    
    fixed_pieces(readRDS("fixed_to_write.rds"))
    build_list(readRDS("buildlist_to_write.rds"))
    player_resources(readRDS("resources_to_write.rds"))
    city_list(readRDS("citylist_to_write.rds"))
    dev_card_counts(readRDS("devcardcounts_to_write.rds"))
  })
  
  ### Define reactive vals ----
  fixed_pieces <- reactiveVal(init_pieces)
  markers_clickable <- reactiveVal(data.frame())
  build_list <- reactiveVal(build_list)
  current_player <- reactiveVal(1)
  dev_card_count <- reactiveVal(0)
  player_resources <- reactiveVal(data.frame(
    matrix(numeric(20), nrow = 4, dimnames = list(
      paste0("P", 1:4), c("wood", "brick", "wool", "wheat", "ore")))
  ))
  dice_rolled <- reactiveVal(FALSE)
  city_list <- reactiveVal(numeric())
  dev_card_counts <- reactiveVal(numeric(4))
  
  ### Render the action buttons ----
  output$whoami <- renderUI({h3(paste("I am player number", player_id))})
  output$presentcurrentplayer <- renderUI({h3(paste("Current player:", current_player()))})
  output$presentplayerresources <- renderTable(player_resources()[current_player(),])
  output$rolldicebutton <- renderUI({
    if(player_id!=current_player()){
      h4(paste0("It's player ", current_player(), "'s turn"))
    } else if(!dice_rolled()){
      actionButton(inputId = "rolldice", "Roll dice", width = "100%")
    }
  })
  output$endturnbutton <- renderUI({
    if(player_id==current_player() & dice_rolled()){
      actionButton(inputId = "finishturn", "End turn", width = "100%")
    }
  })
  output$buydevcard <- renderUI({
    if(player_id==current_player() & dice_rolled()){
      if(all(player_resources()[current_player(), c("wheat", "wool", "ore")]>=1)){
        actionButton(inputId = "buydev", "Buy a dev card", width = "100%")
      }
    }
  })
  output$devcarddetails <- renderUI({
    curplayer_dcc <- dev_card_counts()[current_player()]
    if(curplayer_dcc>0){
      tagList(
        h3("Development cards:"),
        h3(paste("Count:", curplayer_dcc)),
        h4("(Expand with dev card details later)")
      )
    }
  })
  
  ### Plot the thing! ----
  output$world <- renderPlotly({
    set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
                     autorange=FALSE, showspikes=FALSE,
                     showgrid=FALSE, zeroline=FALSE, visible=FALSE)
    ply <- plot_ly() %>%
      add_trace(type="mesh3d", data = globe_plates,
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(globe_plates$faces$color)),
                              maxColorValue = 255),
                lighting=list(diffuse=1),
                hoverinfo="none") %>%
      add_trace(type="mesh3d", data = fixed_pieces(),
                x=~vertices$x, y=~vertices$y, z=~vertices$z,
                i=~faces$i, j=~faces$j, k=~faces$k,
                facecolor=rgb(t(col2rgb(fixed_pieces()$faces$color)),
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
    if(dice_rolled()){
      marker_data_wlabels <- markers_clickable()
      marker_data_wlabels$lab <- ifelse(
        marker_data_wlabels$id%in%build_list()$id & 
          marker_data_wlabels$lab=="settlement", 
        "city", marker_data_wlabels$lab)
      ply <- ply %>% add_trace(type="scatter3d", mode="markers", 
                               data = marker_data_wlabels,
                               x=~x, y=~y, z=~z, key=~id, text=~lab,
                               marker=list(
                                 color="white", opacity=0.1, size=50
                               ),
                               hoverinfo="text",
                               hovertemplate=paste0("Build a %{text}?<extra></extra>"))
    }
    if(input$debug){
      debug_face_spots <- merge(TI_structure$faces, hex_faces)
      debug_face_spots[c("x", "y", "z")] <- debug_face_spots[c("x", "y", "z")] %>%
        apply(1, cart2sphere) %>% `+`(c(2, 0, 0)) %>% apply(2, sphere2cart) %>%
        t() %>% setNames(c("x", "y", "z"))
      ply <- ply %>% add_trace(type="scatter3d", mode="text", data=TI_structure$vertices,
                               x=~x, y=~y, z=~z, text=~id) %>%
        add_trace(type="scatter3d", mode="text", data=TI_structure$edges,
                  x=~x, y=~y, z=~z, text=~id) %>%
        add_trace(type="scatter3d", mode="text", data=debug_face_spots,
                  x=~x, y=~y, z=~z, text=~chit_value, textfont=list(size=18, textposition="bottom center"))
    }
    if(input$showpipvalues){
      face_pip_factor <- c(1:5, 5:1)
      names(face_pip_factor) <- c(2:6, 8:12)
      hex_faces$pip_number <- face_pip_factor[as.character(hex_faces$chit_value)]
      pip_data <- merge(hex_faces, TI_structure$faces)
      pip_data <- pip_data[!is.na(pip_data$pip_number),]
      pip_points <- lapply(seq_len(nrow(pip_data)), function(pip_row_i){
        render_dots(pip_data[pip_row_i,])
      }) %>% 
        do.call(what=rbind)
      pip_points[c("x", "y", "z")] <- pip_points[c("x", "y", "z")] %>%
        apply(1, cart2sphere) %>% `+`(c(2, 0, 0)) %>% apply(2, sphere2cart) %>%
        t() %>% setNames(c("x", "y", "z"))
      ply <- ply %>% add_trace(type="scatter3d", mode="markers", x=~x, y=~y, z=~z,
                               data=pip_points, marker=list(color='red', opacity=0.5),
                               hoverinfo="none")
    }
    ply
  })
  
  ### Watch for dice roll ----
  observeEvent(input$rolldice, {
    if(input$debug)print("Rolling dice!")
    dice_rolled(TRUE)
    value_rolled <- sum(sample(1:6, 2, replace = TRUE))
    value_rolled <- 4
    print(paste("Value rolled:", value_rolled))
    if(value_rolled==7){
      print("Robber activated!")
    } else {
      chosen_faces <- nearby_structures$faces[hex_faces$chit_value==value_rolled,]
      chosen_hexes <- merge(chosen_faces, hex_faces)
      # print(chosen_hexes)
      relevant_vert_ids <- data.frame(
        id=unlist(chosen_hexes$nearest_verts), 
        resource=rep(chosen_hexes$resource, lengths(chosen_hexes$nearest_verts))
      )
      # print("Relevant vert ids:")
      # dput(relevant_vert_ids)
      resources_to_allocate <- merge(build_list(), relevant_vert_ids)
      # print("Resources to allocate:")
      # dput(resources_to_allocate)
      if(nrow(resources_to_allocate)>0){
        resources_to_allocate$owner <- paste0("P", resources_to_allocate$owner)
        # dput(resources_to_allocate)
        player_res <- player_resources()
        # print("Player resources:")
        # dput(player_res)
        for(i in seq_len(nrow(resources_to_allocate))){
          player_res[resources_to_allocate$owner[i], resources_to_allocate$resource[i]] <-
            player_res[resources_to_allocate$owner[i], resources_to_allocate$resource[i]]+1
        }
        player_resources(player_res)
      }
    }
    saveRDS(as.data.frame(player_resources()), "resources_to_write.rds")
    saveRDS(runif(1), file = "out_of_date.rds")
    
    if(input$debug)print("Dice done!")
    # dput(build_list())
    # dput(current_player())
    markers_clickable(get_markers(build_list(), current_player(), 
                                  city_list(), player_resources()))
    if(input$debug)print("Markers updated")
  })
  ### Watch for map clicks ----
  ed <- reactive(event_data(event = "plotly_click"))
  observeEvent(ed(), {
    req(ed()$key)
    if(input$debug)print("Noticed click!")
    
    clicked_point_id <- as.numeric(ed()$key)
    clicked_point_data <- marker_data_unmoved[clicked_point_id, ]
    piece_to_build <- clicked_point_data$lab
    if(clicked_point_id%in%build_list()$id & piece_to_build=="settlement"){
      piece_to_build <- "city"
      city_list(c(city_list(), clicked_point_id))
    }
    
    ### Update resources ----
    player_res <- as.data.frame(player_resources())
    print(piece_to_build)
    if(piece_to_build=="city"){
      player_res[current_player(), "ore"] <-player_res[current_player(), "ore"]-3
      player_res[current_player(), "wheat"] <-player_res[current_player(), "wheat"]-2
    }
    if(piece_to_build=="settlement"){
      settle_res <- c("wheat", "wool", "wood", "brick")
      player_res[current_player(), settle_res] <-player_res[current_player(), settle_res]-1
    }
    if(piece_to_build=="road"){
      road_res <- c("wood", "brick")
      player_res[current_player(), road_res] <-player_res[current_player(), road_res]-1
    }
    player_resources(player_res)
    
    ### Update pieces ----
    cp <- piece_maker(piece_type = piece_to_build, 
                      data_df_row = clicked_point_data,
                      color=player_colors[current_player()])
    fixed_pieces(combine_geoms(list(fixed_pieces(), cp)))
    
    ### Update markers ----
    build_list(rbind(build_list(), data.frame(id=clicked_point_data$id, owner=current_player())))
    # dput(build_list())
    # dput(current_player())
    # dput(city_list())
    markers_clickable(get_markers(build_list(), current_player(), city_list(), player_resources()))
  })
  
  ### Watch for dev card purchase ----
  observeEvent(input$buydev, {
    dev_res <- c("wool", "ore", "wheat")
    player_res <- player_resources()
    player_res[current_player(), dev_res] <- player_res[current_player(), dev_res]-1
    player_resources(player_res)
    dcc <- dev_card_counts()
    dcc[current_player()] <- dcc[current_player()]+1
    dev_card_counts(dcc)
  })
  
  ### Watch for finishturn ----
  observeEvent(input$finishturn, {
    if(input$debug)print("Finishing turn")
    
    # Update player number
    next_player <- ifelse(current_player()==1, 2, 1)
    saveRDS(next_player, file = "current_player.rds")
    
    # Update world
    fixed_to_write <- as.list(fixed_pieces())
    saveRDS(fixed_to_write, file="fixed_to_write.rds")
    
    buildlist_to_write <- as.data.frame(build_list())
    saveRDS(buildlist_to_write, file="buildlist_to_write.rds")
    
    resources_to_write <- as.data.frame(player_resources())
    saveRDS(resources_to_write, file="resources_to_write.rds")
    
    citylist_to_write <- as.data.frame(city_list())
    saveRDS(citylist_to_write, file="citylist_to_write.rds")
    
    devcardcounts_to_write <- dev_card_counts()
    saveRDS(devcardcounts_to_write, file="devcardcounts_to_write.rds")
    
    # Update markers
    markers_clickable(NULL)
    
    # Update game logic
    dice_rolled(FALSE)
    
    # Trigger update
    saveRDS(runif(1), file = "out_of_date.rds")
  })
}

# shinyApp(ui, server)

# browseURL("127.0.0.1:5013")
runApp(list(ui = ui, server = server), port=5013, launch.browser = TRUE)
