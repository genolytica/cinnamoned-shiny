# Function initializing cinnamoned2 universe (packages, persistent variables, 
# etc.)

initPackages <- function(session) {
    # Firstly check packages
    checkPackages(session)
    
    # Initial page loading indicator, until all content is loaded
    ftProgress <- shiny::Progress$new(session,min=0,max=11)
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
    updateFtProgress(value=5,detail="Loading ggplot2")
    require(ggplot2)
    updateFtProgress(value=6,detail="Loading parallel")
    require(parallel)
    updateFtProgress(value=7,detail="Loading RCurl")
    require(RCurl)
    updateFtProgress(value=8,detail="Loading RSQLite")
    require(RSQLite)
    updateFtProgress(value=9,detail="Loading tools")
    require(tools)
    updateFtProgress(value=10,detail="Loading yaml")
    require(yaml)
    updateFtProgress(value=11,detail="Loading tools")
    require(tools)
    #updateFtProgress(value=12,detail="Loading Rdisop")
    #require(Rdisop)
    # highcharter?
}

# Check if bootstrap required
checkPackages <- function(session) {
    # At least shiny must be installed!
    # A good package to check for bootstrapping is shinyjs as it's not related
    # to Bioconductor packages that may be present anyway
    if (!require(shinyjs)) {
        # Sometimes there is a problem with https in virgin VMs...
        source("http://bioconductor.org/biocLite.R")

        if (!require(BiocInstaller)) {
            install.packages("BiocInstaller",
                repos="http://bioconductor.org/packages/3.7/bioc")
            require(BiocInstaller)
        }

        pkgs <- c("RNetCDF","xcms","CAMERA","DT","ggplot2","RCurl",#"Rdisop",
            "tools","RSQLite","shiny","shinyjs","shinythemes","colourpicker",
            "yaml","shinyWidgets","utils")

        # Bootstraping progress bar
        bsProgress <- shiny::Progress$new(session,min=0,max=length(pkgs))
        bsProgress$set(message="Installing package:",value=0)
        on.exit(bsProgress$close())
        
        # Progress update function
        updateBtProgress <- function(value=NULL,detail=NULL) {
            if (is.null(value)) {
                value <- bsProgress$getValue()
                value <- value + 1
            }
            bsProgress$set(value=value,detail=detail)
        }
        
        count <- 0
        for (p in pkgs) {
            count <- count + 1
            if (!require(p,character.only=TRUE)) {
                updateBsProgress(value=count,
                    detail=paste("Installing ",p,collapse=""))
                biocLite(p)
            }
        }
    }
}
