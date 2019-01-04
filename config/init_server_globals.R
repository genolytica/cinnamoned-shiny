# Init globals (don't want to laod these everytime a user connects and they 
# don't take much time to load)

# Load basic packages
require(shiny)
require(shinyjs)
require(colourpicker)

# Load backend functions
source("lib/annotatePeaks.R")
source("lib/diagplots.R")
source("lib/filterBrukerNetCDF.R")
source("lib/general.R")
source("lib/normalizePeaks.R")
source("lib/normalizeSamples.R")
source("lib/queryDBs.R")
source("lib/xcmsPipeline.R")

baseColours <- c("#B40000","#00B400","#0000B4","#B45200","#9B59B6","#21BCBF",
    "#BC4800","#135C34","#838F00","#4900B5")

# Restrict the number of cores dedicated to cinnamoned2
RC <- 0.25

# Error messages
errorMessages <- list(
    projectName=paste("The project name must be smaller than 100", 
        "characters and cannot contain the special characters",
        "\ / @ # $ & * ` ! ( ) % ^ , . < > ? | ' ; [ ] \" or space"),
    filterTimeMin="Minimum time must be an integer greater than 0!",
    filterTimeMax="Maximum time must be an integer greater than 0!",
    xcmsSNR="Signal to noise ratio must be a real number greater than 0!"
)

# Maximum filesize
options(shiny.maxRequestSize=10*1024^3)

LOADED <- FALSE
