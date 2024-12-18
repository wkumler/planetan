/*
library(shiny)

ui <- fluidPage(
  includeCSS("styles.css"),
  includeScript("colorbar.js"),
  sliderInput("slider", "Pick a color!", min = 0, max = 1, value = 1),
)

server <- function(input, output, session) {}

shinyApp(ui, server)
*/


$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'color_choice') {
    var value = event.value;
    var hue = 315-(315 * value); 
    var color = 'hsl(' + hue + ', 100%, 50%)';
    $('.irs-bar').css('background', color);
    $('.irs--shiny .irs-handle').css('background', color);
  }
});
