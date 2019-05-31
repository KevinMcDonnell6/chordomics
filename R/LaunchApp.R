#' Launch Chordomics
#'
#' Function launches Chordomics server and UI
#' @export
#'

launchApp <- function(){
  shinyjs::useShinyjs()
  shiny::shinyApp(server = ChordShinyAppServer,
                  ui = ChordShinyAppUI,
                  onStart =  function() {
                     shiny::addResourcePath("svg-crowbar.js", system.file("www", "svg-crowbar.js", package="chordomics"))
                  },
                  options = list(launch.browser=T))
}

