# Create Chordomics PAckage

library(devtools)
library(roxygen2)
getwd()
# important!
create("chordomicsTest6")

# add R files to R

# sample data for chordomics
Day1 <- read.csv("Day1mix.csv",stringsAsFactors = F)
Day3 <- read.csv("Day3mix.csv",stringsAsFactors = F)
Day7 <- read.csv("Day7mix.csv",stringsAsFactors = F)
exampledata1 <- read.csv("groupfuntest.csv",stringsAsFactors = F)
exampledata2 <- read.csv("groupfuntestall.csv", stringsAsFactors = F)

categories <- readr::read_tsv("COG_data/cognames2003-2014.tab")
funGroups <- readr::read_tsv("COG_data/fun2003-2014.tab")
colnames(categories)[1] <- "COG"  
colnames(funGroups)[1] <- "Code"  

# View(categories)
# View(funGroups)
alltaxa <- read.csv("lineages.csv",stringsAsFactors = F)

# next setwd to the package
setwd("./chordomicsTest6")
# save(categories,file = "cognames2003-2014.tab")
# save(categories,file = "cognames2003-2014.tab")

# save internal data needed
# usethis::use_data(categories,funGroups,exampledata1,exampledata2,alltaxa,internal = T)
usethis::use_data(categories,funGroups,Day1,Day3,Day7,alltaxa,internal = T,overwrite = T)

# devtools::use_data(funGroups,internal = T)
# setwd("./Github/chorddiag")

# add dependencies to "Description - Imports:"
# Add remotes to imports and "Remotes: Github repos" 
# document the package
document()
setwd("..")
getwd()
library(devtools)
install_github("KevinMcDonnell6/chorddiag")

install_github("KevinMcDonnell6/chordomics")
