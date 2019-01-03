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
    timeFilter <- allReactiveVars$timeFilter
    readSpec <- allReactiveVars$readSpec
    findPeaks <- allReactiveVars$findPeaks
    
    # Validators
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
    
    return(list(
        validateTimeFilterMin=validateTimeFilterMin,
        validateTimeFilterMax=validateTimeFilterMax,
        validateXcmsSNR=validateXcmsSNR
    ))
}

analysisTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
        
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
            
    validateTimeFilterMin <- 
        analysisTabPanelReactiveExprs$validateTimeFilterMin
    validateTimeFilterMax <- 
        analysisTabPanelReactiveExprs$validateTimeFilterMax
    validateXcmsSNR <- 
        analysisTabPanelReactiveExprs$validateXcmsSNR
    #TODO: Retrieve all the rest fields defined in analysisTabPanelReactive
    
    # Initialize UI element reactivity  
    analysisTabPanelRenderUI(output,session,allReactiveVars,allReactiveMsgs)
    
    # Set the observers
    # Validators
    observe({
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
    
    # If a validator fails, disable the run button
    observe({
        if (pipelineControl$uiError)
            shinyjs::disable("runPreprocessing")
        else
            shinyjs::enable("runPreprocessing")
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
