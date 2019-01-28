# Init globals (don't want to laod these everytime a user connects and they 
# don't take much time to load)

# Load basic packages
require(shiny)
#require(shinyjs)
#require(shinyWidgets)
#require(colourpicker)

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

# Database paths
METABO_DB <- "data/RFLab_MetaboDB.sqlite"
APP_DB <- "data/cinnamonDB.sqlite"

CURR <- 1

# Error messages
errorMessages <- list(
    projectName=paste("The project name must be smaller than 100", 
        "characters and cannot contain the special characters",
        "\ / @ # $ & * ` ! ( ) % ^ , . < > ? | ' ; [ ] \" or space"),
    filterTimeMin="Minimum time must be an integer greater than 0!",
    filterTimeMax="Maximum time must be an integer greater than 0!",
    filterTimeComparison="Maximum time must be greater than Minimum time!",
    profileStep="Profile reading step must be an integer greater than 0!",
    xcmsSNR="Signal to noise ratio must be a real number greater than 0!",
    xcmsEIBPCSize="EIBPC step size must be a real number greater than 0!",
    xcmsFWHM="Full width at half maximum must be an integer greater than 0!",
    xcmsSigma="Peak model standard deviation (sigma) must be an integer >= 0!",
    xcmsEIBPCSteps="EIBPC steps to combine must be an integer greater than 0!",
    xcmsEIBPCMaxPeaks="Maximum peaks per EIBPC must be an integer greater than 0!",
    mzTol="m/z tolerance must be a real number greater than 0!",
    tSpan="LOESS span must be a real number >= 0!",
    It="Alignment algorithm iterrations must be a real number >= 0!",
    corrFac="LOESS singularity correction factor must be a real number >= 0!",
    cutQ="RT deviation exclusion quantile real number >= 0!",
    iSpan="LOESS span must be a real number >= 0!",
    corrFacNS="Non-standards correction factor must be a real number >= 0!",
    analysisWriteError=paste("An error has been occured while writing results",
		"the database! Please report to the administrator with the analysis",
		"ID above.")
    
)

# Maximum filesize
options(shiny.maxRequestSize=10*1024^3)
