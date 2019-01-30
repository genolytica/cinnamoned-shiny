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
        source("server/databaseTab/databaseBrowseItem.R",local=TRUE)
        source("server/databaseTab/databaseManageItem.R",local=TRUE)
        source("server/databaseTab/databaseModifyItem.R",local=TRUE)
        source("server/databaseTab/databaseSearchItem.R",local=TRUE)
        source("server/runViewerTab/runViewerItem.R",local=TRUE)
        
        # Init packages
        initPackages(session)
        
        ## Make %#^%$^%$@( globals visible AND changeable
        #makeReactiveBinding("someVariable")
        
        # Initialize all the reactive variables used...
        allReactiveVars <- initReactiveVars()
        
        # Analysis
        analysisTabPanelObserve(input,output,session,allReactiveVars)
            
        # Run Viewer
        runViewerTabPanelObserve(input,output,session,allReactiveVars)
        
        # Database
        databaseBrowseTabPanelObserve(input,output,session,allReactiveVars)
        databaseManageTabPanelObserve(input,output,session,allReactiveVars)
        #databaseModifyTabPanelObserve(input,output,session,allReactiveVars)
        databaseSearchTabPanelObserve(input,output,session,allReactiveVars)
    }
)
