# this allows us to use our own version of crowbar and include it in the package
shiny::addResourcePath("svg-crowbar.js", system.file("www", "svg-crowbar.js", package="chordomics"))
