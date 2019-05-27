#!/usr/bin/Rscript
# Use script to run the Chordomics Shiny app (installing any dependencies along the way)

if(!"devtools" %in% rownames(installed.packages())) {
  print("Installing devtools...")
  install.packages("devtools", repos='http://cran.us.r-project.org', dependencies = T)
}

if(!"chorddiag" %in% rownames(installed.packages())) {
  print("Installing chorddiag...")
  devtools::install_github("KevinMcDonnell6/chorddiag", dependencies = T)
}
if(!"chordomics" %in% rownames(installed.packages())) {
  print("Installing chordomics")
  devtools::install_github("KevinMcDonnell6/chordomics", dependencies = T)
}
chordomics::launchApp()


