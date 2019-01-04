analysisTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
    # Retrieve control ractive variables
    pipelineControl <- allReactiveVars$pipelineControl
    timeFilter <- allReactiveVars$timeFilter
    readSpec <- allReactiveVars$readSpec
    findPeaks <- allReactiveVars$findPeaks
    
    runPreprocess <- eventReactive(input$runPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        # Test the button
        pipelineControl$isRunning <- TRUE
        
        # 1. Create the directory structure
        # 2. Move the uploaded files to where the pipeline expects
        # 3. Create the sample info file from the respective inputs
        # 3. Write a JSON (not YAML anymore) configuration file
        # 4. Run xcmsPipeline.R
        
        # Pipeline completed hopefully
        # pipelineControl$isRunning <- FALSE
        
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
        uploadFiles=uploadFiles
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
                    h3("Pipeline running!")
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
}

analysisTabPanelObserve <- function(input,output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    
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
        if (pipelineControl$uiError || !pipelineControl$filesUploaded)
            shinyjs::disable("runPreprocessing")
        else
            shinyjs::enable("runPreprocessing")
    })
    
    # File upload
    observe({
        uploadFiles()
    })
    
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
}
