analysisTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
    # Retrieve control ractive variables
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    #timeFilter <- allReactiveVars$timeFilter
    #readSpec <- allReactiveVars$readSpec
    #findPeaks <- allReactiveVars$findPeaks
    pipelineResult <- allReactiveVars$pipelineResult
    
    runPreprocess <- eventReactive(input$runPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        # Test the button
        pipelineControl$isRunning <- TRUE
        
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
        #xcmsLog <- file(pipelineInput$tmpXcmsLogFile,open="wt")
        #tryCatch(print(xcmsLog),error=function(e) print(e),finally="")
        
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
                headerId="preprocessCurrentFile",
                footerId="preprocessCurrentStep"
            )
        )
        pipelineResult$peaks <- peaks
        
        # 5c. Close the xcmsPipeline log file
        sink(type="message")
        sink()
        #file.copy(pipelineInput$tmpXcmsLogFile,pipelineInput$xcmsLogFile)
        
        # 5d. Save the peaks for backwards compatibility and reusability
        peaks <- peaks$peaks
        pipelineInput$peaksRda <- file.path(pipelineInput$runPath,"peaks.RData")
        save(peaks,file=pipelineInput$peaksRda)

        # Pipeline completed hopefully
        pipelineControl$isRunning <- FALSE
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
        
        # Switch to timefilter status so that the UI can be rendered
        pipelineControl$step <- "timefilter" # Works! Tested.
    })
    
    resetPreprocess <- eventReactive(input$resetPreprocessing,{
        # Get files and parameters from the control variables above
        # With these run the xcmsPipeline.R function
        
        pipelineControl$isRunning <- FALSE
        
        # Reset runtime variables
        allReactiveVars$resetTimefilter()
        allReactiveVars$resetPreprocess()
        
        allReactiveVars$pipelineInput$sampleInfoFile <- NULL
        allReactiveVars$pipelineInput$classes <- NULL
        
        # Reset inputs
        updateNumericInput(session,inputId="filterTimeMin",
            value=allReactiveVars$timeFilter$min)
        updateNumericInput(session,inputId="filterTimeMax",
            value=allReactiveVars$timeFilter$max)
        #TODO: All the rest inputs - DONE?
        updateNumericInput(session,inputId="filterTimeComparison",
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
    })
    
    resetToBack <- eventReactive(input$resetToBack,{
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
        updateNumericInput(session,inputId="filterTimeComparison",
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
    
    resetTimeBoundaries <- eventReactive(input$resetTimeBoundaries,{
    })
    
    proceedToNormalization <- eventReactive(input$proceedToNormalization,{
		pipelineControl$isRunning <- FALSE
		pipelineControl$step <- "normalization"
	})
    
    return(list(
        runPreprocess=runPreprocess,
        resetPreprocess=resetPreprocess,
        resetToBack=resetToBack,
        resetTimeBoundaries=resetTimeBoundaries,
        proceedToNormalization=proceedToNormalization
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
						plotMzrt(peaks[[i]]$rt,peaks[[i]]$mz,
						inten=peaks[[i]]$into,output="shiny")
					})
				}
			}
			else
				output[["rawSpectre_1"]] <- renderPlot({
					plotMzrt(peaks$rt,peaks$mz,output="shiny")
				})
		}
	})
	
	# Time filer boxplots
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
        validatePostTimeFiltersComp=validatePostTimeFiltersComp
    ))
}

analysisTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
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
                        fluidRow(column(1,
							div(
								style="margin-top: 0px;",
								switchInput(
									inputId=paste("reviewTime",i,sep="_"),
									label="Review time bounds",
									onStatus="danger",
									size="small"
								)
							)
							#div(
							#	style="margin-top: -10px;",
							#	checkboxInput(
							#		inputId=paste("reviewTime",i,sep="_"),
							#		label="Review min time (seconds)", 
							#		value=FALSE
							#	)
							#)
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
                        ),column(7," "
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
    resetToBack <- analysisTabPanelReactiveEvents$resetToBack
	resetTimeBoundaries <- analysisTabPanelReactiveEvents$resetTimeBoundaries
	proceedToNormalization <- 
		analysisTabPanelReactiveEvents$proceedToNormalization
    
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

    uploadFiles <- analysisTabPanelReactiveExprs$uploadFiles
    classNames <- analysisTabPanelReactiveExprs$classNames
    spectralReviewPlots <- analysisTabPanelReactiveExprs$spectralReviewPlots
    doTimeFilterReview <- analysisTabPanelReactiveExprs$doTimeFilterReview
    
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
    
    # Act when the run button is pressed
    observe({
        #session$sendCustomMessage("changeProgressHeader",
        #    list(value="Running!"))
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
    
    # Timefilter functions
    observe({
		spectralReviewPlots()
		doTimeFilterReview()
		resetToBack()
        resetTimeBoundaries()
        proceedToNormalization()
	})

}
