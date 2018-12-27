# Intit globals (don't want to laod these everytime a user connects and they 
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
