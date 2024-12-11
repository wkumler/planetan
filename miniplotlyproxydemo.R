library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  actionButton("add_point", label = "Add point")
)

# Server showing basic mesh functionality
server <- function(input, output, session){
  output$plot <- renderPlotly({
    plot_ly(x = 1:10, y = 1:10, z=10:1, mode = "markers", type = "scatter3d",
            marker = list(color = "black"))
  })
  
  observeEvent(event_data("plotly_click"), {
    piece_data <- list(
      vertices=data.frame(
        x=c(0,10,0), 
        y=c(10,0,0), 
        z=c(10,10,0)
      ),
      faces=data.frame(i=0, j=1, k=2)
    )
    # Apparently plotlyProxy can't draw vertical meshes?
    # piece_data <- list(
    #   vertices=data.frame(
    #     x=c(0,10,10), 
    #     y=c(0,10,10), 
    #     z=c(0,10,0)
    #   ),
    #   faces=data.frame(i=0, j=1, k=2)
    # )
    piece_data <- piece_maker("settlement", marker_data_unmoved[4,])
    newtrace <- list(
      type="mesh3d",
      x=piece_data$vertices$x, 
      y=piece_data$vertices$y, 
      z=piece_data$vertices$z,
      i=piece_data$faces$i, 
      j=piece_data$faces$j, 
      k=piece_data$faces$k,
      color="red",
      opacity=1
    )
    print(newtrace)
    plotlyProxy("plot") %>%
      plotlyProxyInvoke("addTraces", list(newtrace))
  })
}

# Server showing how to change a point on click with add/remove traces
server <- function(input, output, session){
  output$plot <- renderPlotly({
    plot_ly(x = 1:10, y = 1:10, mode = "markers", type = "scatter",
            marker = list(color = "red"))
  })
  
  observeEvent(event_data("plotly_click"), {
    event <- event_data("plotly_click")
    print(event)
    
    # Turns the entire trace blue
    # plotlyProxy("plot") %>%
    #   plotlyProxyInvoke("restyle", list(marker = list(color = "blue")), 0)
    
    # Adds/removes an additional trace with each click
    newtrace <- list(x = list(event$x), y = list(event$y), type = "scatter",
                     mode = "markers", marker=list(color="blue"))
    plotlyProxy("plot") %>%
      plotlyProxyInvoke("deleteTraces", list(1))
    plotlyProxy("plot") %>%
      plotlyProxyInvoke("addTraces", newtrace)
  })
}

# Server showing how to extendTraces
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    plot_ly(x = 1:10, y = 1:10, mode = "markers", type = "scatter")
  })
  
  observeEvent(input$add_point, {
    plotlyProxy("plot") %>%
      plotlyProxyInvoke("extendTraces", list(x = list(list(11)), y = list(list(11))), list(0))
  })
}

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    p <- plot_ly() %>%
      add_trace(x = c(10, 0, 0), y = c(0, 10, 0), z=c(0, 0, 10), 
                i = 0, j = 1, k = 2, type = "mesh3d", facecolor="black")
    # p <- p %>% add_trace(type="scatter3d", mode="markers", x=0, y=0, z=0)
    p
  })
  newtrace <- list(
    x=list(list(0, 0, 20)),
    y=list(list(0, 20, 0)),
    z=list(list(20, 0, 0)),
    i=list(list(3)),
    # i=list(list(3)),
    j=list(list(4)),
    k=list(list(5)),
    # color=list(list("red"))# Does NOT work
    facecolor=list(list("red"))# DOES work
  )
  observeEvent(input$add_point, {
    plotlyProxy("plot") %>%
      plotlyProxyInvoke("extendTraces", newtrace, list(0))
    # plotlyProxy("plot") %>%
    #   plotlyProxyInvoke("extendTraces", list(x=list(list(10)), y=list(list(10)), z=list(list(10))), list(1))
  })
}


shinyApp(ui, server)

