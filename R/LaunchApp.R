#' Launch Chordomics
#' 
#' Function launches Chordomics server and UI
#' @export
#' 

launchApp <- function(){
  shinyjs::useShinyjs()
  shiny::shinyApp(server = ChordShinyAppServer, ui = ChordShinyAppUI, options = list(launch.browser=T))
}

