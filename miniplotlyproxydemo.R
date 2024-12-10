library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot")
)

server <- function(input, output, session){
  output$plot <- renderPlotly({
    plot_ly(x = 1:10, y = 1:10, mode = "markers", type = "scatter",
            marker = list(color = "red"))
  })
  
  observeEvent(event_data("plotly_click"), {
    event <- event_data("plotly_click")
    print(event)
    
    # Turns the entire trace blue
    plotlyProxy("plot") %>%
      plotlyProxyInvoke("restyle", list(marker = list(color = "blue")), 0)
    
    # Adds/removes an additional trace with each click
    # newtrace <- list(x = list(event$x), y = list(event$y), type = "scatter",
    #                  mode = "markers", marker=list(color="blue"))
    # plotlyProxy("plot") %>%
    #   plotlyProxyInvoke("deleteTraces", list(1))
    # plotlyProxy("plot") %>%
    #   plotlyProxyInvoke("addTraces", newtrace)
  })
}

shinyApp(ui, server)
