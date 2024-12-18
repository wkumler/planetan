
library(shiny)
library(plotly)

ui <- fillPage(
  actionButton("add_point", "Add point"),
  plotlyOutput("game_world"),
  includeCSS("styles.css")
)

camera_eye <- function() {
  theta <- runif(1, 0, 2 * pi)
  c(1.2 * cos(theta), 1.2 * sin(theta))
}

server <- function(input, output, session){
  observeEvent(input$add_point, {
    print("input$add_point clicked!")
    # newtrace <- list(x = list(0), y = list(0), z = list(15), key = list(0), 
    #                  type = "scatter3d", mode = "markers", 
    #                  marker = list(color = "red", opacity = 1, size = 50))
    # plotlyProxy("game_world") %>% plotlyProxyInvoke("addTraces", newtrace)
    
    robber_init <- readRDS("game_files/QUNWYD/robber_data.rds")
    newtrace <- list(
      type="mesh3d",
      x=as.list(robber_init$vertices$x),
      y=as.list(robber_init$vertices$y),
      z=as.list(robber_init$vertices$z),
      i=as.list(robber_init$faces$i),
      j=as.list(robber_init$faces$j),
      k=as.list(robber_init$faces$k),
      facecolor=as.list(robber_init$faces$color),
      hoverinfo="none"
    )
    plotlyProxy("game_world") %>% plotlyProxyInvoke("addTraces", list(newtrace))
  })
  
  output$game_world <- renderPlotly({
    print("Re-rendering game_world")
    globe_plates <- readRDS("game_files/QUNWYD/globe_plates.rds")
    set_axis <- list(range=max(abs(globe_plates$vertices))*c(-1, 1),
                     autorange=FALSE, showspikes=FALSE,
                     showgrid=FALSE, zeroline=FALSE, visible=FALSE)
    start_camera <- camera_eye()
    print(start_camera)
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
        camera=list(eye=list(x=start_camera[1], y=start_camera[2], z=0.8)),
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
    
    return(ply)
  })
}

shinyApp(ui, server)
