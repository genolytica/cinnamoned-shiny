# server.R

# Load required libraries
source("config/init_server_globals.R")

shinyServer(
    function(input,output,session) {
        # Load init packages script
        source("config/init_packages.R")
        # Load SeqCVIBE libs
        source("server/reactiveVars.R",local=TRUE)
        source("server/analysisTab/analysisItem.R",local=TRUE)
        source("server/databaseTab/databaseItem.R",local=TRUE)
        
        # Init packages
        initPackages(session)
        
        ## Make %#^%$^%$@( globals visible AND changeable
        #makeReactiveBinding("someVariable")
        
        # Initialize all the reactive variables used...
        allReactiveVars <- initReactiveVars()
        # ...and reactive messages
        allReactiveMsgs <- initReactiveMsgs()
        
        # Analysis
        analysisTabPanelObserve(input,output,session,allReactiveVars,
            allReactiveMsgs)
        
        # Database
        databaseTabPanelObserve(input,output,session,allReactiveVars,
            allReactiveMsgs)
    }
)
