analysisTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
    # Retrieve control ractive variables
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    #timeFilter <- allReactiveVars$timeFilter
    #readSpec <- allReactiveVars$readSpec
    #findPeaks <- allReactiveVars$findPeaks
    pipelineResults <- allReactiveVars$pipelineResults
    
    runPreprocess <- eventReactive(input$runPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        # Show progress stuff
        shinyjs::show("progressWrapper")
        shinyjs::html("analysisProgress","Pipeline running!")
        
        # Disable class name and other inputs
        lapply(1:length(pipelineInput$filenames),function(i,n) {
            shinyjs::disable(paste("className_",i,sep=""))
        },pipelineInput$filenames)
        shinyjs::disable("xcmsDefaultParameters")
        shinyjs::disable("filterTimeMin")
        shinyjs::disable("filterTimeMax")
        shinyjs::disable("projectFiles")
        shinyjs::disable("sampleInfoFile")
        shinyjs::disable("resetPreprocessing")
        shinyjs::disable("resetNormalization")

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
            plottype="png",
            shinyProgressData=list(
                session=session,
                progressId="preprocessProgressBar",
                progressTotal=16,
                textId="pre"
            )
        )
        
        # 5c. Close the xcmsPipeline log file
        sink(type="message")
        sink()
        
        # 5d. Save the peaks for backwards compatibility and reusability
        peaks <- peaks$peaks
        pipelineResults$peaks <- peaks
        pipelineInput$peaksRda <- file.path(pipelineInput$runPath,"peaks.RData")
        save(peaks,file=pipelineInput$peaksRda)

        # Pipeline completed hopefully
        shinyjs::html("analysisProgress","Preprocess complete!")
        
        # Enable class name and other inputs
        lapply(1:length(pipelineInput$filenames),function(i,n) {
            shinyjs::enable(paste("className_",i,sep=""))
        },pipelineInput$filenames)
        shinyjs::enable("xcmsDefaultParameters")
        shinyjs::enable("filterTimeMin")
        shinyjs::enable("filterTimeMax")
        shinyjs::enable("projectFiles")
        shinyjs::enable("sampleInfoFile")
        shinyjs::enable("resetPreprocessing")
        shinyjs::enable("resetNormalization")
        
        # Switch to timefilter status so that the UI can be rendered
        pipelineControl$step <- "timefilter" # Works! Tested.
    })
    
    resetPreprocess <- eventReactive(input$resetPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        # Reset runtime variables
        allReactiveVars$resetTimefilter()
        allReactiveVars$resetPreprocess()
        allReactiveVars$resetNormalization()
        
        allReactiveVars$pipelineInput$sampleInfoFile <- NULL
        allReactiveVars$pipelineInput$classes <- NULL
        
        # Reset inputs
        updateNumericInput(session,inputId="filterTimeMin",
            value=allReactiveVars$timeFilter$min)
        updateNumericInput(session,inputId="filterTimeMax",
            value=allReactiveVars$timeFilter$max)
        #TODO: All the rest inputs - DONE?
        updateNumericInput(session,inputId="profileStep",
            value=allReactiveVars$readSpec$profstep)
        updateNumericInput(session,inputId="xcmsSNR",
            value=allReactiveVars$findPeaks$snthresh)
        updateNumericInput(session,inputId="xcmsEIBPCSize",
            value=allReactiveVars$findPeaks$step)
        updateNumericInput(session,inputId="xcmsFWHM",
            value=allReactiveVars$findPeaks$fwhm)
        updateNumericInput(session,inputId="xcmsSigma",
            value=allReactiveVars$findPeaks$sigma)
        updateNumericInput(session,inputId="xcmsEIBPCSteps",
            value=allReactiveVars$findPeaks$steps)
        updateNumericInput(session,inputId="xcmsEIBPCMaxPeaks",
            value=allReactiveVars$findPeaks$max)
    })
    
    resetToBack <- eventReactive(input$resetToBack,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        # Reset runtime variables
        allReactiveVars$resetTimefilter()
        allReactiveVars$resetPreprocess()
        allReactiveVars$resetNormalization()
        
        # Reset inputs
        updateNumericInput(session,inputId="filterTimeMin",
            value=allReactiveVars$timeFilter$min)
        updateNumericInput(session,inputId="filterTimeMax",
            value=allReactiveVars$timeFilter$max)
        updateNumericInput(session,inputId="profileStep",
            value=allReactiveVars$readSpec$profstep)
        updateNumericInput(session,inputId="xcmsSNR",
            value=allReactiveVars$findPeaks$snthresh)
        updateNumericInput(session,inputId="xcmsEIBPCSize",
            value=allReactiveVars$findPeaks$step)
        updateNumericInput(session,inputId="xcmsFWHM",
            value=allReactiveVars$findPeaks$fwhm)
        updateNumericInput(session,inputId="xcmsSigma",
            value=allReactiveVars$findPeaks$sigma)
        updateNumericInput(session,inputId="xcmsEIBPCSteps",
            value=allReactiveVars$findPeaks$steps)
        updateNumericInput(session,inputId="xcmsEIBPCMaxPeaks",
            value=allReactiveVars$findPeaks$max)
            
        # Go to the first page
        pipelineControl$step <- "preprocess"
    })
    
    resetNormalization <- eventReactive(input$resetNormalization,{
        # Reset runtime variables
        allReactiveVars$resetTimefilter()
        allReactiveVars$resetPreprocess()
        allReactiveVars$resetNormalization()
        
        # Reset inputs
        updateSelectInput(session,inputId="method",
            selected=allReactiveVars$normPeaks$method)
        updateSelectInput(session,inputId="correctfor",
            selected=allReactiveVars$normPeaks$correctfor)
        updateNumericInput(session,inputId="mztol",
            value=allReactiveVars$normPeaks$mztol)
        updateCheckboxInput(session,inputId="diagPlotsInclude",
            value=allReactiveVars$normPeaks$diagPlotsInclude)
        updateSelectInput(session,inputId="export",
            selected=allReactiveVars$normPeaks$export)
        updateNumericInput(session,inputId="tspan",
            value=allReactiveVars$normPeaks$tspan)
        updateNumericInput(session,inputId="it",
            value=allReactiveVars$normPeaks$it)
        updateNumericInput(session,inputId="corrfac",
            value=allReactiveVars$normPeaks$corrfac)
        updateNumericInput(session,inputId="cutq",
            value=allReactiveVars$normPeaks$cutq)
        updateSelectInput(session,inputId="diagPlots",
            selected=allReactiveVars$normPeaks$diagPlots)
        updateNumericInput(session,inputId="ispan",
            value=allReactiveVars$normPeaks$ispan)
        updateNumericInput(session,inputId="corrfacNS",
            value=allReactiveVars$normPeaks$corrfacNS)
    })
    
    resetTimeBoundaries <- eventReactive(input$resetTimeBoundaries,{
        lapply(1:length(pipelineInput$filenames),function(i) {
            updateNumericInput(
                session=session,
                inputId=paste("reviewMinTime",i,sep="_"),
                value=600
            )
            updateNumericInput(
                session=session,
                inputId=paste("reviewMaxTime",i,sep="_"),
                value=3000
            )
        })
    })
    
    proceedToNormalization <- eventReactive(input$proceedToNormalization,{
        # Store the time boundaries to
        # pipelineInput$refinedTimeBoundaries
        # pipelineInput$filenames must not be NULL at this point
        pipelineInput$refinedTimeBoundaries <-
            lapply(1:length(pipelineInput$filenames),function(i) {
                 return(c(
                    as.numeric(input[[paste("reviewMinTime",i,sep="_")]]),
                    as.numeric(input[[paste("reviewMaxTime",i,sep="_")]])
                 ))
            })
        pipelineControl$step <- "normalization"
    })
    
    runNormalization <- eventReactive(input$runNormalization,{
        pipelineInput$normLogFile <- 
            file.path(pipelineInput$scriptPath,"norm.Rout")
        normLog <- file(pipelineInput$normLogFile,open="wt")
        
        # Show progress stuff
        shinyjs::show("progressWrapperN")
        shinyjs::html("normalizationProgress","Normalization running!")
        
        # Disable controls while running
        normInputs <- c("method","correctfor","mztol","diagPlotsInclude",
           "export","tspan","it","corrfac","cutq","diagPlots","ispan",
           "corrfacNS")
        sapply(normInputs,shinyjs::disable)
        
        sink(normLog)
        sink(normLog,type="message")
        
        norm <- normalizeSamples(
            peaks=isolate(pipelineResults$peaks),
            dbdata=METABO_DB,
            method=as.character(input$method),
            normalize="rlm",
            correctfor=as.character(input$correctfor),
            time.range=pipelineInput$refinedTimeBoundaries,
            tol=as.numeric(input$mztol),
            tspan=as.numeric(input$tspan),
            ispan=as.numeric(input$ispan),
            tit=as.numeric(input$tit),
            cutq=as.numeric(input$cutq),
            corrfac=as.numeric(input$corrfac),
            cutrat=as.numeric(input$corrfacNS),
            export=file.path(pipelineInput$runPath,"norm_output.txt"),
            diagplot=pipelineInput$diagPathNormalization,
            plottype="png",
            export.type=as.character(input$export),
            shinyProgressData=list(
                session=session,
                progressId="normalizationProgressBar",
                progressTotal=3,
                textId="norm"
            )
        )
        
        sink(type="message")
        sink()
        
        pipelineResults$norm <- norm
        pipelineInput$normRda <- file.path(pipelineInput$runPath,"norm.RData")
        save(norm,file=pipelineInput$normRda)
       
        # Re-enable controls
        sapply(normInputs,shinyjs::enable)
        
        pipelineResults$currentIndex <- 1
        pipelineControl$step <- "result"
    })
    
    discardAnalysis <- eventReactive(input$discardAnalysis,{
        # TODO: Reset EVERYTHING (see functions above) and also files
        
        # Go to first page
        pipelineControl$step <- "preprocess"
    })
    
    saveAnalysis <- eventReactive(input$saveAnalysis,{
        # TODO: Write parameters to SQLite and display a message (essentially
        # update the button with "Saved" and disable). Discard becomes "New"
    })
    
    getDiagTab <- eventReactive(input$analysisDiagnosticPlots,{
        if (pipelineControl$step == "result") {
            for (i in 1:length(pipelineInput$filenames)) {
                cond <- isolate({
                    input$analysisDiagnosticPlots == paste("diagTab",i,sep="_")
                })
                if (cond)
                    pipelineResults$currentIndex <- i
            }
        }
    })
    
    return(list(
        runPreprocess=runPreprocess,
        resetPreprocess=resetPreprocess,
        resetToBack=resetToBack,
        resetNormalization=resetNormalization,
        resetTimeBoundaries=resetTimeBoundaries,
        proceedToNormalization=proceedToNormalization,
        runNormalization=runNormalization,
        discardAnalysis=discardAnalysis,
        saveAnalysis=saveAnalysis,
        getDiagTab=getDiagTab
    ))
}

analysisTabPanelReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    pipelineResults <- allReactiveVars$pipelineResults
    timeFilter <- allReactiveVars$timeFilter
    readSpec <- allReactiveVars$readSpec
    findPeaks <- allReactiveVars$findPeaks
    normPeaks <- allReactiveVars$normPeaks
    
    # Validators
    
    # Preprocessing validators
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
    validateTimeFilterComp <- reactive({
        tComp <-  
            as.numeric(input$filterTimeMax)-as.numeric(input$filterTimeMin)
        if (tComp < 0 || is.na(tComp)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateProfStep <- reactive({
        pStep <- as.numeric(input$profileStep)
        if (pStep <= 0 || is.na(pStep)) {
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
        if (snr <= 0 || is.na(snr)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateXcmsEIBPCSize <- reactive({
        eibpcsize <- as.numeric(input$xcmsEIBPCSize)
        if (eibpcsize <= 0 || is.na(eibpcsize)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateXcmsFWHM <- reactive({
        fwhm <- as.numeric(input$xcmsFWHM)
        if (fwhm <= 0 || is.na(fwhm)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateXcmsSigma <- reactive({
        sigma <- as.numeric(input$xcmsSigma)
        if (sigma < 0 || is.na(sigma)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateXcmsEIBPCSteps <- reactive({
        eibpcsteps <- as.numeric(input$xcmsEIBPCSteps)
        if (eibpcsteps <= 0 || is.na(eibpcsteps)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    validateXcmsEIBPCMaxPeaks <- reactive({
        maxpeaks <- as.numeric(input$xcmsEIBPCMaxPeaks)
        if (maxpeaks <= 0 || is.na(maxpeaks)) {
            pipelineControl$uiError <- TRUE
            return(TRUE)
        }

        else {
            pipelineControl$uiError <- FALSE
            return(FALSE)
        }
    })
    # Post time filter validators
    validatePostTimeFiltersMin <- reactive({
        if (!is.null(pipelineInput$filenames)
            && pipelineControl$step=="timefilter"
            && !is.null(input$reviewMinTime_1)) {
            valids <- lapply(1:length(pipelineInput$filenames),function(i) {
                postMin <- as.numeric(input[[paste("reviewMinTime",i,sep="_")]])
                if (postMin < 0 || is.na(postMin)) {
                    pipelineControl$uiError <- TRUE
                    return(TRUE)
                }
                else {
                    pipelineControl$uiError <- FALSE
                    return(FALSE)
                }
            })
            return(unlist(valids))
        }
    })
    
    validatePostTimeFiltersMax <- reactive({
        if (!is.null(pipelineInput$filenames)
            && pipelineControl$step=="timefilter"
            && !is.null(input$reviewMaxTime_1)) {
            valids <- lapply(1:length(pipelineInput$filenames),function(i) {
                postMax <- as.numeric(input[[paste("reviewMaxTime",i,sep="_")]])
                if (postMax < 0 || is.na(postMax)) {
                    pipelineControl$uiError <- TRUE
                    return(TRUE)
                }
                else {
                    pipelineControl$uiError <- FALSE
                    return(FALSE)
                }
            })
            return(unlist(valids))
        }
    })
    
    validatePostTimeFiltersComp <- reactive({
        if (!is.null(pipelineInput$filenames)
            && pipelineControl$step=="timefilter"
            && !is.null(input$reviewMaxTime_1)) {
            valids <- lapply(1:length(pipelineInput$filenames),function(i) {
                tComp <- as.numeric(input[[paste("reviewMaxTime",i,sep="_")]])-
                    as.numeric(input[[paste("reviewMinTime",i,sep="_")]])
                if (tComp < 0 || is.na(tComp)) {
                    pipelineControl$uiError <- TRUE
                    return(TRUE)
                }
                else {
                    pipelineControl$uiError <- FALSE
                    return(FALSE)
                }
            })
            return(unlist(valids))
        }
    })
    
    # Normalization Validators
    validatemzTol <- reactive({
        mztol <- as.numeric(input$mztol)
        if (mztol <= 0 || is.na(mztol)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })

    validatetSpan <- reactive({
        tspan <- as.numeric(input$tspan)
        if (tspan < 0 || is.na(tspan)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })

    validateIt <- reactive({
        it <- as.numeric(input$tit)
        if (it < 0 || is.na(it)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })

    validateCorrFac <- reactive({
        corrfac <- as.numeric(input$corrfac)
        if (corrfac < 0 || is.na(corrfac)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })

    validateCutQ <- reactive({
        cutq <- as.numeric(input$cutq)
        if (cutq < 0 || is.na(cutq)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })
    
    validateiSpan <- reactive({
        ispan <- as.numeric(input$ispan)
        if (ispan < 0 || is.na(ispan)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })
    
    validateCorrFacNS <- reactive({
        corrfacns <- as.numeric(input$corrfacNS)
        if (corrfacns < 0 || is.na(corrfacns)) {
            pipelineControl$uiError <- TRUE
        return(TRUE)
        }
        else {
            pipelineControl$uiError <- FALSE
        return(FALSE)
        }
    })


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
    
    # Spectral review plots
    spectralReviewPlots <- reactive({
        peaks <- pipelineResults$peaks
        if (!is.null(peaks)) {
            if (is.list(peaks)) {
                for (i in 1:length(peaks)) {
                    output[[paste("rawSpectre",i,sep="_")]] <- renderPlot({
                        plot.mzrt(peaks[[i]]$rt,peaks[[i]]$mz,
                        inten=peaks[[i]]$into,output="shiny")
                    })
                }
            }
            else
                output[["rawSpectre_1"]] <- renderPlot({
                    plot.mzrt(peaks$rt,peaks$mz,output="shiny")
                })
        }
    })
    
    # Time filter controls
    doTimeFilterReview <- reactive({
        if (!is.null(pipelineInput$filenames) 
            && pipelineControl$step=="timefilter"
            && !is.null(input$reviewTime_1)) {
            lapply(1:length(pipelineInput$filenames),function(i) {
                if (input[[paste("reviewTime",i,sep="_")]]) {
                    timeFilter$do <- TRUE
                    shinyjs::enable(paste("reviewMinTime",i,sep="_"))
                    shinyjs::enable(paste("reviewMaxTime",i,sep="_"))
                }
                else {
                    timeFilter$do <- FALSE
                    shinyjs::disable(paste("reviewMinTime",i,sep="_"))
                    shinyjs::disable(paste("reviewMaxTime",i,sep="_"))
                }
            })
        }
    })
    
    # Final diagnostic plots - alignment
    finalAlignmentPlots <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("finalAlignment",i,sep="_")]] <- renderPlot({
                    plot.match(
                        pd[[i]]$rtref,
                        pd[[i]]$ref$mz[pd[[i]]$match.ref[[i]]$ref.idx],
                        pd[[i]]$rtcor,
                        pd[[i]]$peaks[[i]]$mz[pd[[i]]$match.ref[[i]]$new.idx],
                        pd[[i]]$rtnew,
                        pd[[i]]$peaks[[i]]$mz[pd[[i]]$match.ref[[i]]$new.idx],
                        output="shiny"
                    )
                })
            }
        }
    })
    
    # Final diagnostic plots - deviation
    finalDeviationPlots <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("finalDeviation",i,sep="_")]] <- renderPlot({
                    plot.rtdev(
                        x=pd[[i]]$x,
                        y=pd[[i]]$y,
                        l=pd[[i]]$iset,
                        exclude=pd[[i]]$badrt.ref,
                        output="shiny"
                    )
                })
            }
        }
    })
    
    # Final diagnostic plots - boxplot
    finalBoxplots <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("finalBoxplot",i,sep="_")]] <- renderPlot({
                    boxplot.mat(
                        cbind(pd[[i]]$intref,pd[[i]]$intnew,pd[[i]]$intcor),
                        name=c("Reference","Raw","Normalized"),
                        output="shiny"
                    )
                })
            }
        }
    })
    
    # Final diagnostic plots - raw intensities
    finalRawint <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("finalRawint",i,sep="_")]] <- renderPlot({
                    plot.rvn(
                        pd[[i]]$a[,1],
                        pd[[i]]$b[,1],
                        pd[[i]]$aa,
                        pd[[i]]$bb,
                        lim=pd[[i]]$cutrat,
                        output="shiny"
                    )
                })
            }
        }
    })
    
    # Final diagnostic plots - normalized intensities
    finalNormint <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("finalNormint",i,sep="_")]] <- renderPlot({
                    plot.rvn(
                        pd[[i]]$a[,2],
                        pd[[i]]$b[,2],
                        pd[[i]]$aa,
                        pd[[i]]$bb,
                        lim=pd[[i]]$cutrat,
                        output="shiny"
                    )
                })
            }
        }
    })
    
    # Final diagnostic plots - raw intensities
    finalStdint <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("finalStdint",i,sep="_")]] <- renderPlot({
                    plot.rvn(
                        pd[[i]]$aa,
                        pd[[i]]$bb,
                        lim=pd[[i]]$cutrat,
                        output="shiny"
                    )
                })
            }
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
            },
            result = {
                return("result")
            }
        )
    })
    outputOptions(output,"panelStatus",suspendWhenHidden=FALSE)
    
    #pollData <- reactivePoll(1000,session,checkFunc = function() {
    #    if (!is.null(pipelineInput$xcmsLogFile)) {
    #        if (file.exists(pipelineInput$xcmsLogFile))
    #            file.info(pipelineInput$xcmsLogFile)$mtime[1]
    #        else
    #            ""
    #    }
    #    else 
    #        ""
    #},valueFunc = function() {
    #    if (!is.null(pipelineInput$xcmsLogFile)) {
    #        if (file.exists(pipelineInput$xcmsLogFile))
    #            readLines(pipelineInput$xcmsLogFile)
    #    }
    #})
    
    return(list(
        validateProjectName=validateProjectName,
        validateTimeFilterMin=validateTimeFilterMin,
        validateTimeFilterMax=validateTimeFilterMax,
        validateTimeFilterComp=validateTimeFilterComp,
        validateProfStep=validateProfStep,
        validateXcmsSNR=validateXcmsSNR,
        validateXcmsEIBPCSize=validateXcmsEIBPCSize,
        validateXcmsFWHM=validateXcmsFWHM,
        validateXcmsSigma=validateXcmsSigma,
        uploadFiles=uploadFiles,
        validateXcmsEIBPCSteps=validateXcmsEIBPCSteps,
        validateXcmsEIBPCMaxPeaks=validateXcmsEIBPCMaxPeaks,
        classNames=classNames,
        spectralReviewPlots=spectralReviewPlots,
        doTimeFilterReview=doTimeFilterReview,
        validatePostTimeFiltersMin=validatePostTimeFiltersMin,
        validatePostTimeFiltersMax=validatePostTimeFiltersMax,
        validatePostTimeFiltersComp=validatePostTimeFiltersComp,
        validatemzTol=validatemzTol,
        validatetSpan=validatetSpan,
        validateIt=validateIt,
        validateCorrFac=validateCorrFac,
        validateCutQ=validateCutQ,
        validateiSpan=validateiSpan,
        validateCorrFacNS=validateCorrFacNS,
        finalAlignmentPlots=finalAlignmentPlots,
        finalDeviationPlots=finalDeviationPlots,
        finalBoxplots=finalBoxplots,
        finalRawint=finalRawint,
        finalNormint=finalNormint,
        finalStdint=finalStdint
    ))
}

analysisTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    pipelineResults <- allReactiveVars$pipelineResults
    timeFilter <- allReactiveVars$timeFilter
    
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
    
    # Test table
    output$sampleInfoTable <- renderTable({
        if (!is.null(pipelineInput$filenames)
            && !any(pipelineInput$classes=="") 
            && !any(is.null(pipelineInput$classes))
            && length(pipelineInput$filenames)==length(pipelineInput$classes))
            data.frame(
                Filename=pipelineInput$filenames,
                Class=pipelineInput$classes
            )
         else
            data.frame(Filename=NULL,Class=NULL)
    })
    
    # Timefilter page
    output$spectralInspection <- renderUI({
        if (!is.null(pipelineInput$filenames)) {
            lapply(1:length(pipelineInput$filenames),function(i,n) {
                fluidRow(column(12,
                    wellPanel(
                        h4("Spectral plot for ",basename(n[i])),
                        br(),
                        plotOutput(paste("rawSpectre",i,sep="_"),
                            height="600px"),
                        br(),
                        fluidRow(column(2,
                            div(
                                style="font-weight:600; font-size:1em;",
                                "Review time boundaries"
                            ),
                            div(
                                style="margin-top: 10px;",
                                switchInput(
                                    inputId=paste("reviewTime",i,sep="_"),
                                    onStatus="danger",
                                    label="Switch"
                                )
                            )
                        ),column(2,
                            disabled(numericInput(
                                inputId=paste("reviewMinTime",i,sep="_"),
                                label="Review min time (seconds)", 
                                value=timeFilter$min,
                                min=0
                            )),
                            div(id=paste("filterTimeMinError",i,sep="_"),
                                class="input-error",
                                errorMessages$filterTimeMin)
                        ),column(2,
                            disabled(numericInput(
                                inputId=paste("reviewMaxTime",i,sep="_"),
                                label="Review max time (seconds)", 
                                value=timeFilter$max,
                                min=0
                            )),
                            div(id=paste("filterTimeMaxError",i,sep="_"),
                                class="input-error",
                                errorMessages$filterTimeMax)
                        ),column(6," "
                        ),
                        div(
                            id=paste("filterTimeCompError",i,sep="_"),
                            class="input-error",
                            errorMessages$filterTimeComparison
                        )),
                        class="well-panel"
                    )
                ))
            },pipelineInput$filenames)
        }
    })
    
    # Results page
    output$resultsPage <- renderUI({
        if (pipelineControl$step=="result" && !is.null(pipelineResults$norm)) {
            fluidRow(column(12,
            
            fluidRow(column(8,
                wellPanel(
                    fluidRow(column(12,
                        h4("Summary"),
                        hr(),
                        div(
                            style="font-size=1.3em",
                            "Your analysis with project ID",
                            tags$span(
                                style="font-weight:600; color:#D70000",
                                pipelineInput$currentRunId
                            ),
                            "has been successfully completed! The results can ",
                            "be reviewed in the tabs below. You can also",
                            "choose one of the following actions."
                        )
                    )),
                    fluidRow(br()),
                    fluidRow(column(8,
                        div(
                            class="pull-left",
                            downloadButton(
                                outputId="exportResults",
                                label="Download results",
                                class="btn-black"
                            )
                        )
                    ),column(2,
                        div(
                            class="pull-left",
                            actionButton(
                                inputId="discardAnalysis",
                                label="Discard analysis",
                                icon=icon("ban")
                            )
                        )
                    ),column(2,
                        div(
                            class="pull-right",
                            actionButton(
                                inputId="saveAnalysis",
                                label="Save analysis",
                                class="btn-primary",
                                icon=icon("floppy-o")
                            )
                        )
                    )),
                    class="well-panel"
                )
            ),column(4,
                wellPanel(
                    fluidRow(column(12,
                        h4("Reference dataset match stats"),
                        hr(),
                        div(
                            style="font-size:1.1em; font-weight:600",
                            pipelineInput$filenames[
                                pipelineResults$currentIndex]," - ",
                            pipelineInput$classes[pipelineResults$currentIndex]
                        ),
                        div(
                            tags$strong(pipelineResults$norm$pct[
                                pipelineResults$currentIndex,"total"]),
                            "% of new m/z matching with reference"
                        ),
                        div(
                            tags$strong(pipelineResults$norm$pct[
                                pipelineResults$currentIndex,"is"]),
                            "% of new sample IS matching with reference"
                        ),
                        div(
                            tags$strong(pipelineResults$norm$pct[
                                pipelineResults$currentIndex,"is_rt"]),
                            "% of new sample IS used for RT correction"
                        ),
                        div(
                            tags$strong(pipelineResults$norm$pct[
                                pipelineResults$currentIndex,"is_inten"]),
                            "% of new sample IS use for intensity normalization"
                        )
                    )),
                    class="well-panel"
                )
            )),
            fluidRow(column(12,
                wellPanel(
                    h4("Diagnostics"),
                    hr(),
                    do.call(tabsetPanel,c(
                        id="analysisDiagnosticPlots",
                        lapply(1:length(pipelineInput$filenames),function(i,n) {
                            tabPanel(
                                h5(n[i]),
                                fluidRow(br()),
                                wellPanel(
                                    fluidRow(column(12,
                                        div(
                                            style=paste(
                                                "font-size: 1.1em;",
                                                "margin-bottom:10px"
                                            ),
                                            "Spectral alignment for ",
                                            tags$span(style="font-weight:600",
                                                n[i])
                                        ),
                                        plotOutput(paste("finalAlignment",i,
                                            sep="_"),height="1200px")
                                    )),
                                    class="well-panel"
                                ),
                                wellPanel(
                                    fluidRow(column(12,
                                        div(
                                            style=paste(
                                                "font-size: 1.1em;",
                                                "margin-bottom:10px"
                                            ),
                                            "Retention time and intensity ",
                                            "normalization for ",
                                            tags$span(style="font-weight:600",
                                                n[i])
                                        )
                                    )),
                                    fluidRow(column(8,
                                        plotOutput(paste("finalDeviation",i,
                                            sep="_"),height="600px")
                                    ),column(4,
                                        plotOutput(paste("finalBoxplot",i,
                                            sep="_"),height="600px")
                                    )),
                                    class="well-panel"
                                ),
                                wellPanel(
                                    fluidRow(column(12,
                                        div(
                                            style=paste(
                                                "font-size: 1.1em;",
                                                "margin-bottom:10px"
                                            ),
                                            "Scatter and Mean-Difference ",
                                            "plots for ",
                                            tags$span(style="font-weight:600",
                                                n[i])
                                        ),
                                        plotOutput(paste("finalRawint",i,
                                            sep="_"),height="400px"),
                                        fluidRow(br()),
                                        plotOutput(paste("finalNormint",i,
                                            sep="_"),height="400px"),
                                        fluidRow(br()),
                                        plotOutput(paste("finalStdint",i,
                                            sep="_"),height="400px")
                                    )),
                                    class="well-panel"
                                ),
                                value=paste("diagTab",i,sep="_")
                            )
                        },pipelineInput$filenames)
                    )),
                    class="well-panel"
                )
            ))
            
            ))
        }
    })
}

analysisTabPanelObserve <- function(input,output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    pipelineResults <- allReactiveVars$pipelineResults
    
    # Initialize observing reactive events
    analysisTabPanelReactiveEvents <- 
        analysisTabPanelEventReactive(input,output,session,
            allReactiveVars,allReactiveMsgs)
    
    runPreprocess <- analysisTabPanelReactiveEvents$runPreprocess
    resetPreprocess <- analysisTabPanelReactiveEvents$resetPreprocess
    resetToBack <- analysisTabPanelReactiveEvents$resetToBack
    resetNormalization <- analysisTabPanelReactiveEvents$resetNormalization
    resetTimeBoundaries <- analysisTabPanelReactiveEvents$resetTimeBoundaries
    proceedToNormalization <- 
        analysisTabPanelReactiveEvents$proceedToNormalization
    runNormalization <- analysisTabPanelReactiveEvents$runNormalization
    discardAnalysis <- analysisTabPanelReactiveEvents$discardAnalysis
    saveAnalysis <- analysisTabPanelReactiveEvents$saveAnalysis
    getDiagTab <- analysisTabPanelReactiveEvents$getDiagTab

    # Initialize observing reactive expressions
    analysisTabPanelReactiveExprs <- 
        analysisTabPanelReactive(input,output,session,allReactiveVars,
            allReactiveMsgs)
    
    # Preprocessing validators    
    validateProjectName <- 
        analysisTabPanelReactiveExprs$validateProjectName
    validateTimeFilterMin <- 
        analysisTabPanelReactiveExprs$validateTimeFilterMin
    validateTimeFilterMax <- 
        analysisTabPanelReactiveExprs$validateTimeFilterMax
    validateTimeFilterComp <- 
      analysisTabPanelReactiveExprs$validateTimeFilterComp
    validateProfStep <- 
      analysisTabPanelReactiveExprs$validateProfStep
    validateXcmsSNR <- 
        analysisTabPanelReactiveExprs$validateXcmsSNR
    validateXcmsEIBPCSize <- 
      analysisTabPanelReactiveExprs$validateXcmsEIBPCSize
    validateXcmsFWHM <- 
      analysisTabPanelReactiveExprs$validateXcmsFWHM
    validateXcmsSigma <- 
      analysisTabPanelReactiveExprs$validateXcmsSigma
    validateXcmsEIBPCSteps <- 
      analysisTabPanelReactiveExprs$validateXcmsEIBPCSteps
    validateXcmsEIBPCMaxPeaks <- 
      analysisTabPanelReactiveExprs$validateXcmsEIBPCMaxPeaks
    
    # Timefilter validator
    validatePostTimeFiltersMin <- 
        analysisTabPanelReactiveExprs$validatePostTimeFiltersMin
    validatePostTimeFiltersMax <- 
        analysisTabPanelReactiveExprs$validatePostTimeFiltersMax
    validatePostTimeFiltersComp <-
        analysisTabPanelReactiveExprs$validatePostTimeFiltersComp
    
    # Normalization validator
    validatemzTol <- 
        analysisTabPanelReactiveExprs$validatemzTol
    validatetSpan <- 
        analysisTabPanelReactiveExprs$validatetSpan
    validateIt <- 
        analysisTabPanelReactiveExprs$validateIt
    validateCorrFac <- 
        analysisTabPanelReactiveExprs$validateCorrFac
    validateCutQ <-
        analysisTabPanelReactiveExprs$validateCutQ
    validateiSpan <-
        analysisTabPanelReactiveExprs$validateiSpan
    validateCorrFacNS <- 
        analysisTabPanelReactiveExprs$validateCorrFacNS


    uploadFiles <- analysisTabPanelReactiveExprs$uploadFiles
    classNames <- analysisTabPanelReactiveExprs$classNames
    spectralReviewPlots <- analysisTabPanelReactiveExprs$spectralReviewPlots
    doTimeFilterReview <- analysisTabPanelReactiveExprs$doTimeFilterReview
    finalAlignmentPlots <- analysisTabPanelReactiveExprs$finalAlignmentPlots
    finalDeviationPlots <- analysisTabPanelReactiveExprs$finalDeviationPlots
    finalBoxplots <- analysisTabPanelReactiveExprs$finalBoxplots
    finalRawint <- analysisTabPanelReactiveExprs$finalRawint
    finalNormint <- analysisTabPanelReactiveExprs$finalNormint
    finalStdint <- analysisTabPanelReactiveExprs$finalStdint
    
    # Initialize UI element reactivity  
    analysisTabPanelRenderUI(output,session,allReactiveVars,allReactiveMsgs)
    
    # Set the observers
    # Validators
    observe({
        if (validateProjectName())
            shinyjs::show("projectNameError")
        else
            shinyjs::hide("projectNameError")
            
        if (validateTimeFilterMin())
            shinyjs::show("filterTimeMinError")
        else
            shinyjs::hide("filterTimeMinError")
            
        if (validateTimeFilterMax())
            shinyjs::show("filterTimeMaxError")
        else
            shinyjs::hide("filterTimeMaxError")
        
        if (validateTimeFilterComp())
            shinyjs::show("filterTimeCompError")
        else
            shinyjs::hide("filterTimeCompError")
      
        if (validateProfStep())
            shinyjs::show("profileStepError")
        else
            shinyjs::hide("profileStepError")
      
        if (validateXcmsSNR())
            shinyjs::show("xcmsSNRError")
        else
            shinyjs::hide("xcmsSNRError")
            
        if (validateXcmsEIBPCSize())
            shinyjs::show("xcmsEIBPCSizeError")
        else
            shinyjs::hide("xcmsEIBPCSizeError")
      
        if (validateXcmsFWHM())
            shinyjs::show("xcmsFWHMError")
        else
            shinyjs::hide("xcmsFWHMError")
      
        if (validateXcmsSigma())
            shinyjs::show("xcmsSigmaError")
        else
            shinyjs::hide("xcmsSigmaError")
      
        if (validateXcmsEIBPCSteps())
            shinyjs::show("xcmsEIBPCStepsError")
        else
            shinyjs::hide("xcmsEIBPCStepsError")
      
        if (validateXcmsEIBPCMaxPeaks())
            shinyjs::show("xcmsEIBPCMaxPeaksError")
        else
            shinyjs::hide("xcmsEIBPCMaxPeaksError")
        
        # Time filter Page Validator Observers
        
        if (any(validatePostTimeFiltersMin())) {
            valids <- validatePostTimeFiltersMin()
            lapply(1:length(valids),function(i,v) {
                if (v[i])
                    shinyjs::show(paste("filterTimeMinError",i,sep="_"))
                else
                    shinyjs::hide(paste("filterTimeMinError",i,sep="_"))
            },valids)
        }
        else
            lapply(1:length(pipelineInput),function(i) {
                shinyjs::hide(paste("filterTimeMinError",i,sep="_"))
            })
        
        if (any(validatePostTimeFiltersMax())) {
            valids <- validatePostTimeFiltersMax()
            lapply(1:length(valids),function(i,v) {
                if (v[i])
                    shinyjs::show(paste("filterTimeMaxError",i,sep="_"))
                else
                    shinyjs::hide(paste("filterTimeMaxError",i,sep="_"))
            },valids)
        }
        else
            lapply(1:length(pipelineInput),function(i) {
                shinyjs::hide(paste("filterTimeMaxError",i,sep="_"))
            })
        
        if (any(validatePostTimeFiltersComp())) {
            valids <- validatePostTimeFiltersComp()
            lapply(1:length(valids),function(i,v) {
                if (v[i])
                    shinyjs::show(paste("filterTimeCompError",i,sep="_"))
                else
                    shinyjs::hide(paste("filterTimeCompError",i,sep="_"))
            },valids)
        }
        else
            lapply(1:length(pipelineInput),function(i) {
                shinyjs::hide(paste("filterTimeCompError",i,sep="_"))
            })

        # Normalization Page Validator Observers
        
        if (validatemzTol())
            shinyjs::show("mzTolError")
        else
            shinyjs::hide("mzTolError")

        if (validatetSpan())
            shinyjs::show("tSpanError")
        else
            shinyjs::hide("tSpanError")
        if (validateIt())
            shinyjs::show("itError")
        else
            shinyjs::hide("itError")
        if (validateCorrFac())
            shinyjs::show("corrFacError")
        else
            shinyjs::hide("corrFacError")
        if (validateCutQ())
            shinyjs::show("cutQError")
        else
            shinyjs::hide("cutQError")
        if (validateiSpan())
            shinyjs::show("iSpanError")
        else
            shinyjs::hide("iSpanError")
        if (validateCorrFacNS())
            shinyjs::show("corrFacNSError")
        else
            shinyjs::hide("corrFacNSError")

    })
    
    # If a preprocessing validator fails or requirements not met, disable the 
    # run preprocessing button
    observe({
        classesOK <- !is.null(pipelineInput$classes) &&
            all(pipelineInput$classes != "") &&
            length(pipelineInput$classes) == length(pipelineInput$filenames)
        if (pipelineControl$uiError || !pipelineControl$filesUploaded 
            || !classesOK ) {
            shinyjs::disable("runPreprocessing")
        }
        else{
            shinyjs::enable("runPreprocessing")
        }
    })
    
    # File upload
    observe({
        uploadFiles()
        classNames()
    })
    
    # Act when the runPreprocessing button is pressed
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
    
    # Act when the reset button in the first page is pressed
    observe({
        resetPreprocess()
    })
     observe({
        resetNormalization()
    })
     
    ## ! Changed this to conditionalPanel to be more Shiny native
    ## Disable Normalization inputs when "use defaults" is selected
    #observe({
    #    normInputs <- c("method","correctfor","mztol","diagPlotsInclude",
    #       "export","tspan","it","corrfac","cutq","diagPlots","ispan",
    #       "corrfacNS")
    #    
    #    enabledIf <- function(inputID) {
    #        shinyjs::toggleState(inputID, input$normalizationParameters != "defaults")
    #    }
    #    
    #    lapply(normInputs, enabledIf)
    #})
   
    # Timefilter functions
    observe({
        spectralReviewPlots()
        doTimeFilterReview()
        resetToBack()
    })
    
    # For some reason, resetTimeBoundaries and proceedToNormalization need 
    # their own observer...
    observe({
        resetTimeBoundaries()
    })
    observe({
        proceedToNormalization()
    })
    
    # If a normalization validator fails or requirements not met, disable the 
    # run normalization button
    observe({
        if (pipelineControl$uiError || is.null(pipelineResults$peaks)) {
            shinyjs::disable("runNormalization")
        }
        else {
            shinyjs::enable("runNormalization")
        }
    })
    
    # Act when the run normalization button is pressed
    observe({
        tryCatch({
            shinyjs::disable("runNormalization")
            runNormalization()
        },error=function(e) {
            print(e)
        },
        finally={
            shinyjs::enable("runNormalization")
        })
    })
    
    # Observe reporting page buttons
    observe({
        saveAnalysis()
    })
    observe({
        discardAnalysis()
    })
    observe({
        getDiagTab()
    })
    
    # Observe reporting page
    observe({
        finalAlignmentPlots()
        finalDeviationPlots()
        finalBoxplots()
        finalRawint()
        finalNormint()
        finalStdint()
    })
}
