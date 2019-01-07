analysisTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
    # Retrieve control ractive variables
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    #timeFilter <- allReactiveVars$timeFilter
    #readSpec <- allReactiveVars$readSpec
    #findPeaks <- allReactiveVars$findPeaks
    
    runPreprocess <- eventReactive(input$runPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        # Test the button
        pipelineControl$isRunning <- TRUE
        
        # 1. Create the directory structure
        # 1a. Define the structure
        pipelineInput$currentRunId <- format(Sys.time(),"%d%m%Y%H%M%S")
        pipelineInput$runPath <- file.path(pipelineInput$basePath,
            pipelineInput$currentRunId)
        pipelineInput$dataPath <- file.path(pipelineInput$runPath,"data")
        pipelineInput$dataPathRaw <- file.path(pipelineInput$dataPath,"raw")
        pipelineInput$dataPathTrunc <- file.path(pipelineInput$dataPath,"trunc")
        pipelineInput$diagPath <- file.path(pipelineInput$runPath,"diagnostic")
        pipelineInput$diagPathPreprocess <-
            file.path(pipelineInput$diagPath,"preprocess")
        pipelineInput$diagPathNormalization <-
            file.path(pipelineInput$diagPath,"normalization")
        pipelineInput$scriptPath <- file.path(pipelineInput$runPath,"scripts")
        # 1b. Create the structure
        if (!dir.exists(pipelineInput$runPath)) {
            dir.create(pipelineInput$runPath,recursive=TRUE,mode="0755")
            dir.create(pipelineInput$dataPathRaw,recursive=TRUE,mode="0755")
            dir.create(pipelineInput$dataPathTrunc,recursive=TRUE,mode="0755")
            dir.create(pipelineInput$diagPathPreprocess,recursive=TRUE,
                mode="0755")
            dir.create(pipelineInput$diagPathNormalization,recursive=TRUE,
                mode="0755")
            dir.create(pipelineInput$scriptPath,recursive=TRUE,mode="0755")
        }
        else { # Support for future reruns of a failed r
            if (!dir.exists(pipelineInput$dataPathRaw))
                dir.create(pipelineInput$dataPathRaw,recursive=TRUE,mode="0755")
            if (!dir.exists(pipelineInput$dataPathTrunc))
                dir.create(pipelineInput$dataPathTrunc,recursive=TRUE,
                    mode="0755")
            if (!dir.exists(pipelineInput$diagPathPreprocess))
                dir.create(pipelineInput$diagPathPreprocess,recursive=TRUE,
                    mode="0755")
            if (!dir.exists(pipelineInput$diagPathNormalization))
                dir.create(pipelineInput$diagPathPNormalization,recursive=TRUE,
                    mode="0755")
            if (!dir.exists(pipelineInput$scriptPath))
                dir.create(pipelineInput$scriptPath,recursive=TRUE,mode="0755")
        }
        
        # 2. Move the uploaded files to where the pipeline expects - time?
        if (!is.null(pipelineInput$uploadedFiles)) {
            destination <- file.path(pipelineInput$dataPathRaw,
                pipelineInput$filenames)
            file.copy(pipelineInput$uploadedFiles,destination)
        }
        
        # 3. Create the sample info file from the respective inputs
        # Engage button not supposed to be available prior to proper class 
        # definition
        siDf <- data.frame( 
            Filename=pipelineInput$filenames,
            Class=pipelineInput$classes
        )
        # TODO: This should enter a tryCatch wrapper with some human fiendly
        # error if something goes wrong.
        pipelineInput$sampleInfoFile <- 
            file.path(pipelineInput$runPath,"sample_info.txt")
        write.table(siDf,pipelineInput$sampleInfoFile,sep="\t",quote=FALSE,
            row.names=FALSE)
        
        # 4. Write a YAML configuration file (maybe later switch to JSON)
        preParams <- list(
            filter=isolate(reactiveValuesToList(
                allReactiveVars$timeFilter)),
            read=isolate(reactiveValuesToList(
                allReactiveVars$readSpec)),
            find=isolate(reactiveValuesToList(
                allReactiveVars$findPeaks)),
            group=isolate(reactiveValuesToList(
                allReactiveVars$groupPeaks)),
            retcor=isolate(reactiveValuesToList(
                allReactiveVars$retcor)),
            extract=isolate(reactiveValuesToList(
                allReactiveVars$extractPeaks)),
            annotate=isolate(reactiveValuesToList(
                allReactiveVars$annotatePeaks))
        )
        print(preParams)
        pipelineInput$xcmsParamFile <- 
            file.path(pipelineInput$scriptPath,"xcms.yml")
        write_yaml(preParams,pipelineInput$xcmsParamFile)
        
        # 5. Run xcmsPipeline.R
        # 5a. Open the xcmsPipeline log file (to be polled in the UI)
        pipelineInput$xcmsLogFile <- 
            file.path(pipelineInput$scriptPath,"xcms.Rout")
        xcmsLog <- file(pipelineInput$xcmsLogFile,open="wt")
        sink(xcmsLog)
        sink(xcmsLog,type="message")
        
        # 5b. Run actual pipeline
        peaks <- xcmsPipeline(
            path.to.raw=pipelineInput$dataPathRaw,
            info.file=pipelineInput$sampleInfoFile,
            param.file=pipelineInput$xcmsParamFile,
            path.to.trunc=pipelineInput$dataPathTrunc,
            annotate=FALSE,
            persample=TRUE,
            multicore=TRUE,
            plotspec=pipelineInput$diagPathPreprocess,
            plottype="png"
        )
        
        # 5c. Close the xcmsPipeline log file
        sink(type="message")
        sink()
        
        # 5d. Save the peaks for backwards compatibility and reusability
        peaks <- peaks$peaks
        pipelineInput$peaksRda <- file.path(pipelineInput$runPath,"peaks.RData")
        save(peaks,file=pipelineInput$peaksRda)

        # Pipeline completed hopefully
        pipelineControl$isRunning <- FALSE
        
        # Switch to timefilter status so that the UI can be rendered
        # pipelineControl$step <- "timefilter" # Works! Tested.
    })
    
    resetPreprocess <- eventReactive(input$resetPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        pipelineControl$isRunning <- FALSE
        
        # Reset runtime variables
        allReactiveVars$resetTimefilter()
        allReactiveVars$resetPreprocess()
        
        # Reset inputs
        updateNumericInput(session,inputId="filterTimeMin",
            value=allReactiveVars$timeFilter$min)
        updateNumericInput(session,inputId="filterTimeMax",
            value=allReactiveVars$timeFilter$max)
        #TODO: All the rest inputs
        
    })
    
    return(list(
        runPreprocess=runPreprocess,
        resetPreprocess=resetPreprocess
    ))
}

analysisTabPanelReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    timeFilter <- allReactiveVars$timeFilter
    readSpec <- allReactiveVars$readSpec
    findPeaks <- allReactiveVars$findPeaks
    
    # Validators
    validateProjectName <- reactive({
        n <- as.character(input$projectName)
        if (is.character(n)) {
            v <- grep("[!@#$%^&*\\\\()+\\/\"'<>,.;:|\\[\\]{}\\s]",n,perl=TRUE)
            if (length(v) > 0 || nchar(n) > 100) {
                pipelineControl$uiError <- TRUE
                return(TRUE)
            }
            else {
                pipelineControl$uiError <- FALSE
                return(FALSE)
            }
        }
        
    })
    validateTimeFilterMin <- reactive({
        tMin <- as.numeric(input$filterTimeMin)
        if (tMin < 0 || is.na(tMin)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
            
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateTimeFilterMax <- reactive({
        tMax <- as.numeric(input$filterTimeMax)
        if (tMax < 0 || is.na(tMax)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
            
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateXcmsSNR <- reactive({
        snr <- as.numeric(input$xcmsSNR)
        if (snr < 0 || is.na(snr)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
            
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    #TODO: All the rest validators
    
    # Uploaded files
    uploadFiles <- reactive({
        if (!is.null(input$projectFiles)) {
            pipelineInput$uploadedFiles <- input$projectFiles$datapath
            pipelineInput$filenames <- input$projectFiles$name
            pipelineControl$filesUploaded <- TRUE
        }
    })
    
    # Sample classes
    classNames <- reactive({
        pipelineInput$classes <- 
            sapply(1:length(pipelineInput$filenames),function(i) {
                return(as.character(input[[paste("className_",i,sep="")]]))
            })
    })
    
    #xcmsLogFileData <- reactive({
    #    if (!is.null(pipelineInput$xcmsLogFile)) {
    #        reactiveFileReader(1000,session,
    #            pipelineInput$xcmsLogFile,function(x) {
    #                allReactiveVars$runtimeMessages$preprocess <- readLines(x)
    #            }
    #        )
    #    }
    #})
    
    # Conditional panel status according to analysis status
    output$panelStatus <- reactive({
        switch(pipelineControl$step,
            preprocess = {
                return("preprocess")
            },
            timefilter = {
                return("timefilter")
            },
            normalization = {
                return("normalization")
            }
        )
    })
    outputOptions(output,"panelStatus",suspendWhenHidden=FALSE)
    
    return(list(
        validateProjectName=validateProjectName,
        validateTimeFilterMin=validateTimeFilterMin,
        validateTimeFilterMax=validateTimeFilterMax,
        validateXcmsSNR=validateXcmsSNR,
        uploadFiles=uploadFiles,
        classNames=classNames#,
        #xcmsLogFileData=xcmsLogFileData
    ))
}

analysisTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
        
    output$analysisProgress <- renderUI({
        switch(pipelineControl$step,
            preprocess = {
                if (pipelineControl$isRunning) {
                    # Code to observe file etc.
                    
                    # Button test!
                    div(
                        h3("Pipeline running!"),
                        div(class="run-progress",
                            verbatimTextOutput("runProgress")
                        )
                    )
                }
                else {
                    h3("Analysis progress will be displayed here")
                }
            },
            timefilter = {
                # Stub
            },
            normalization = {
                # Stub
            }
        )
    })
    
    # xcms log file div contents
    output$runProgress <- renderText({
        xcmsLogFileData <- NULL
        if (!is.null(pipelineInput$xcmsLogFile))
            xcmsLogFileData <- reactiveFileReader(1000,session,
                pipelineInput$xcmsLogFile,readLines)
        if (!is.null(xcmsLogFileData))
            xcmsLogFileData()
    })
    
    # Filenames and classes
    output$sampleInfoEdit <- renderUI({
        if (!is.null(pipelineInput$filenames)) {
            lapply(1:length(pipelineInput$filenames),function(i,n) {
                fluidRow(column(6,
                    disabled(textInput(
                        inputId=paste("sampleName_",i,sep=""),
                        value=n[i],
                        label=""
                    ))
                ),column(6,
                    textInput(
                        inputId=paste("className_",i,sep=""),
                        placeholder="Enter class name",
                        label=""
                    )
                ))
            },pipelineInput$filenames)
        }
    })
    
    ## Test table
    #output$sampleInfoTable <- renderTable({
    #    if (length(pipelineInput$filenames)==length(pipelineInput$classes)
    #        && all(pipelineInput$classes != ""))
    #        data.frame(
    #            Filename=pipelineInput$filenames,
    #            Class=pipelineInput$classes
    #        )
    #})
}

analysisTabPanelObserve <- function(input,output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    
    # Initialize observing reactive events
    analysisTabPanelReactiveEvents <- 
        analysisTabPanelEventReactive(input,output,session,
            allReactiveVars,allReactiveMsgs)
    
    runPreprocess <- analysisTabPanelReactiveEvents$runPreprocess
    resetPreprocess <- analysisTabPanelReactiveEvents$resetPreprocess
    
    # Initialize observing reactive expressions
    analysisTabPanelReactiveExprs <- 
        analysisTabPanelReactive(input,output,session,allReactiveVars,
            allReactiveMsgs)
        
    validateProjectName <- 
        analysisTabPanelReactiveExprs$validateProjectName
    validateTimeFilterMin <- 
        analysisTabPanelReactiveExprs$validateTimeFilterMin
    validateTimeFilterMax <- 
        analysisTabPanelReactiveExprs$validateTimeFilterMax
    validateXcmsSNR <- 
        analysisTabPanelReactiveExprs$validateXcmsSNR
    #TODO: Retrieve all the rest fields defined in analysisTabPanelReactive
    
    uploadFiles <- analysisTabPanelReactiveExprs$uploadFiles
    classNames <- analysisTabPanelReactiveExprs$classNames
    #xcmsLogFileData <- analysisTabPanelReactiveExprs$xcmsLogFileData
    
    # Initialize UI element reactivity  
    analysisTabPanelRenderUI(output,session,allReactiveVars,allReactiveMsgs)
    
    # Set the observers
    # Validators
    observe({
        if (validateProjectName())
            shinyjs::show("projectNameError")
        else
            shinyjs::hide("projectNameError")
            
        if (validateTimeFilterMax())
            shinyjs::show("filterTimeMaxError")
        else
            shinyjs::hide("filterTimeMaxError")
        
        if (validateTimeFilterMin())
            shinyjs::show("filterTimeMinError")
        else
            shinyjs::hide("filterTimeMinError")
            
        if (validateTimeFilterMax())
            shinyjs::show("filterTimeMaxError")
        else
            shinyjs::hide("filterTimeMaxError")
            
        if (validateXcmsSNR())
            shinyjs::show("xcmsSNRError")
        else
            shinyjs::hide("xcmsSNRError")
            
        # TODO: All the rest validators
    })
    
    # If a validator fails or requirements not met, disable the run button
    observe({
        classesOK <- !is.null(pipelineInput$classes) &&
            all(pipelineInput$classes != "") &&
            length(pipelineInput$classes) == length(pipelineInput$filenames)
        if (pipelineControl$uiError || !pipelineControl$filesUploaded 
            || !classesOK)
            shinyjs::disable("runPreprocessing")
        else
            shinyjs::enable("runPreprocessing")
    })
    
    # File upload
    observe({
        uploadFiles()
        classNames()
    })
    
    #observe({
    #    xcmsLogFileData()
    #})
    
    # Act when the run button is pressed
    observe({
        tryCatch({
            shinyjs::disable("runPreprocessing")
            runPreprocess()
        },error=function(e) {
            #print(e)
        },
        finally={
            shinyjs::enable("runPreprocessing")
        })
    })
    
    # Act when the reset button is pressed
    observe({
        resetPreprocess()
    })
    
    # More
}
