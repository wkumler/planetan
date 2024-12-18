$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'color_choice') {
    var value = event.value;
    var hue = 315-(315 * value); 
    var color = 'hsl(' + hue + ', 100%, 50%)';
    $('.irs-bar').css('background', color);
    $('.irs--shiny .irs-handle').css('background', color);
  }
});