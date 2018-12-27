# Function initializing cinnamoned2 universe (packages, persistent variables, 
# etc.)
initPackages <- function(session) {
    # Initial page loading indicator, until all content is loaded
    ftProgress <- shiny::Progress$new(session,min=0,max=10 )
    ftProgress$set(message="Starting:",value=0)
    on.exit(ftProgress$close())
    
    # Progress update function
    updateFtProgress <- function(value=NULL,detail=NULL) {
        if (is.null(value)) {
            value <- ftProgress$getValue()
            value <- value + 1
        }
        ftProgress$set(value=value,detail=detail)
    }
    
    # Load packages
    updateFtProgress(value=1,detail="Loading DT")
    require(DT)
    updateFtProgress(value=2,detail="Loading RNetCDF")
    require(RNetCDF)
    updateFtProgress(value=3,detail="Loading xcms")
    require(xcms)
    updateFtProgress(value=4,detail="Loading CAMERA")
    require(CAMERA)
    updateFtProgress(value=5,detail="Loading Rdisop")
    require(Rdisop)
    updateFtProgress(value=6,detail="Loading ggplot2")
    require(ggplot2)
    updateFtProgress(value=7,detail="Loading parallel")
    require(parallel)
    updateFtProgress(value=8,detail="Loading RCurl")
    require(RCurl)
    updateFtProgress(value=9,detail="Loading RSQLite")
    require(RSQLite)
    updateFtProgress(value=10,detail="Loading tools")
    require(tools)
    # highcharter?
}
