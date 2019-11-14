analysisTabPanelEventReactive <- function(input,output,session,
    allReactiveVars) {
    # Retrieve control ractive variables
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    timeFilter <- allReactiveVars$timeFilter
    readSpec <- allReactiveVars$readSpec
    findPeaks <- allReactiveVars$findPeaks
    normPeaks <- allReactiveVars$normPeaks
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
        pipelineInput$projectName <- as.character(input$projectName)
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
        tryCatch({
            write.table(siDf,pipelineInput$sampleInfoFile,sep="\t",quote=FALSE,
                row.names=FALSE)
            }, error=function(e) {
              message("following error: ",e)
              shinyjs::html("analysisProgress","Writing Sample Info table")
              shinyjs::html("sampleInfoError","Error writing Sample Info file!")
              pipelineControl$uiError <- TRUE
        },finally="")
        
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
        close(xcmsLog)
        
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
        
        ########################################################################
        # Reset the timebar for later
		updateShinyProgressBar(
			shinyProgressData=list(
                session=session,
                progressId="preprocessProgressBar",
                progressTotal=16,
                textId="pre"
            ),
			pbValue=0,
			headerMsg="",
			footerMsg=""
		)
		########################################################################
        
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
        updateSelectInput(session,inputId="normalize",
            selected=allReactiveVars$normPeaks$normalize)
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
        
        # Update reactive vars
        normPeaks$method=as.character(input$method)
        normPeaks$correctfor=as.character(input$correctfor)
        normPeaks$mztol=as.numeric(input$mztol)
        normPeaks$export=as.character(input$export)
        normPeaks$tspan=as.numeric(input$tspan)
        normPeaks$tit=as.numeric(input$tit)
        normPeaks$corrfac=as.numeric(input$corrfac)
        normPeaks$cutq=as.numeric(input$cutq)
        normPeaks$normalize=as.character(input$normalize)
        normPeaks$ispan=as.numeric(input$ispan)
        normPeaks$corrfacNS=as.numeric(input$corrfacNS)
        
        # Show progress stuff
        shinyjs::show("progressWrapperN")
        shinyjs::html("normalizationProgress","Normalization running!")
        
        # Disable controls while running
        normInputs <- c("method","correctfor","mztol","diagPlotsInclude",
           "export","tspan","it","corrfac","cutq","normalize","ispan",
           "corrfacNS")
        sapply(normInputs,shinyjs::disable)
        
        sink(normLog)
        sink(normLog,type="message")
        
        #noNorm <- normalizeSamples(
        #    peaks=isolate(pipelineResults$peaks),
        #    dbdata=METABO_DB,
        #    method=as.character(input$method),
        #    normalize=as.character(input$normalize),
        #    correctfor="none",
        #    time.range=pipelineInput$refinedTimeBoundaries,
        #    tol=as.numeric(input$mztol),
        #    tspan=as.numeric(input$tspan),
        #    ispan=as.numeric(input$ispan),
        #    tit=as.numeric(input$tit),
        #    cutq=as.numeric(input$cutq),
        #    corrfac=as.numeric(input$corrfac),
        #    cutrat=as.numeric(input$corrfacNS),
        #    export=file.path(pipelineInput$runPath,"no_norm_output.txt"),
        #    diagplot=NULL,
        #    export.type=as.character(input$export),
        #    shinyProgressData=list(
        #        session=session,
        #        progressId="normalizationProgressBar",
        #        progressTotal=3,
        #        textId="norm"
        #    )
        #)
        
        norm <- normalizeSamples(
            peaks=isolate(pipelineResults$peaks),
            dbdata=METABO_DB,
            method=as.character(input$method),
            normalize=as.character(input$normalize),
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
        close(normLog)
        
        pipelineResults$norm <- norm
        pipelineInput$normRda <- file.path(pipelineInput$runPath,"norm.RData")
        save(norm,file=pipelineInput$normRda)
       
        # Re-enable controls
        sapply(normInputs,shinyjs::enable)
        
        ########################################################################
        # Reset the timebar for later
		updateShinyProgressBar(
			shinyProgressData=list(
                session=session,
                progressId="normalizationProgressBar",
                progressTotal=3,
                textId="norm"
            ),
			pbValue=0,
			headerMsg="",
			footerMsg=""
		)
		########################################################################
        
        # So as to record the change, talk to the fileInput and then be true
        # again upon hitting the discard/new analysis button
        pipelineControl$newAnalysis <- FALSE
        pipelineResults$currentIndex <- 1
        pipelineControl$step <- "result"
    })
    
    preDiscardAnalysis <- eventReactive(input$discardAnalysis, {
		if (!pipelineControl$analysisSaved 
			&& !pipelineControl$analysisDiscarded)
			showModal(modalDialog(
				title="Confirm analysis discard!",
				"Are you sure you want to discard the present analysis? This ",
				"action cannot be undone!",
				easyClose=FALSE,
				footer=tagList(
					modalButton("Cancel",icon=icon("ban")),
					actionButton("confirmAnalysisDiscard","Discard",
						class="btn-danger",icon=icon("exclamation-triangle"))
				)
			))
    })
    
    discardAnalysis <- eventReactive(input$confirmAnalysisDiscard,{
		# Delete run directory if analysis not saved
        if (!pipelineControl$analysisDiscarded) {
            ex <- unlink(pipelineInput$runPath,recursive=TRUE,force=TRUE)
            pipelineControl$analysisDiscarded <- TRUE
            updateActionButton(session,"discardAnalysis",
                label="Start new analysis",icon=icon("star"))
            updateActionButton(session,"saveAnalysis",
				label="Analysis discarded!",icon=icon("frown-o"))
			shinyjs::disable("saveAnalysis")
			removeModal()
		}
	})
    
    newAnalysis <- eventReactive(input$discardAnalysis,{
		if (pipelineControl$analysisSaved || pipelineControl$analysisDiscarded){ 
			# Go to first page
			pipelineControl$firstRun <- FALSE
			pipelineControl$newAnalysis <- TRUE
			
			# Reset runtime variables
			allReactiveVars$resetAll()
			
			# All uploaded files must be deleted
			print(input$projectFiles$datapath)
			unlink(input$projectFiles$datapath,recursive=TRUE,force=TRUE)
			
			# Reset ALL inputs
			updateTextInput(session,inputId="projectName",value="")
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
			updateSelectInput(session,inputId="normalize",
				selected=allReactiveVars$normPeaks$normalize)
			updateNumericInput(session,inputId="ispan",
				value=allReactiveVars$normPeaks$ispan)
			updateNumericInput(session,inputId="corrfacNS",
				value=allReactiveVars$normPeaks$corrfacNS)
			
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
			
			# Revert Analysis Progress well-panel
			shinyjs::hide("progressWrapper")
			shinyjs::html("analysisProgress",
				"Analysis progress will be displayed here")
		}
    })
    
    saveAnalysis <- eventReactive(input$saveAnalysis,{
        # Run parameters list
        runParameters <- list(
            ref_run_id=paste('\'',pipelineInput$currentRunId,'\'',sep=""),
            xcms_filter_do=ifelse(timeFilter$do,1,0),
            xcms_filter_min=timeFilter$min,
            xcms_filter_max=timeFilter$max,
            xcms_read_profstep=readSpec$profstep,
            xcms_read_profmethod=paste('\'',readSpec$profmethod,'\'',sep=""),
            xcms_find_snthresh=findPeaks$snthresh,
            xcms_find_step=findPeaks$step,
            xcms_find_fwhm=findPeaks$fwhm,
            xcms_find_sigma=findPeaks$sigma,
            xcms_find_steps=findPeaks$steps,
            xcms_find_max=findPeaks$max,
            xcms_find_mzdiff=findPeaks$mzdiff,
            norm_method=paste('\'',normPeaks$method,'\'',sep=""),
            norm_tol=normPeaks$mztol,
            norm_correctfor=paste('\'',normPeaks$correctfor,'\'',sep=""),
            norm_export=paste('\'',normPeaks$export,'\'',sep=""),
            norm_diagplot=ifelse(normPeaks$diagPlotsInclude,1,0),
            norm_tspan=normPeaks$tspan,
            norm_tit=normPeaks$tit,
            norm_normalize=paste('\'',normPeaks$normalize,'\'',sep=""),
            norm_corrfac=normPeaks$corrfac,
            norm_ispan=normPeaks$ispan,
            norm_cutrat=normPeaks$corrfacNS,
            norm_cutq=normPeaks$cutq,
            norm_times=paste('\'',paste(pipelineInput$refinedTimeBoundaries,
                collapse=","),'\'',sep="")
        )
            
        # Run info list
        runInfo <- list(
            run_id=paste('\'',pipelineInput$currentRunId,'\'',sep=""),
            project_name=paste('\'',pipelineInput$projectName,'\'',sep=""),
            project_path=paste('\'',pipelineInput$runPath,'\'',sep=""),
            diagnostic_preprocess_path=paste('\'',
                pipelineInput$diagPathPreprocess,'\'',sep=""),
            diagnostic_normalization_path=paste('\'',
                pipelineInput$diagPathNormalization,'\'',sep=""),
            yaml_file=paste('\'',pipelineInput$xcmsParamFile,'\'',sep=""),
            class_file=paste('\'',pipelineInput$sampleInfoFile,'\'',sep=""),
            rdata_peaks_file=paste('\'',pipelineInput$peaksRda,'\'',sep=""),
            rdata_norm_file=paste('\'',pipelineInput$normRda,'\'',sep=""),
            result_file=paste('\'',file.path(pipelineInput$runPath,
                "norm_output.txt"),'\'',sep=""),
            peak_detection_script=NULL,
            normalization_script=NULL
        )
        
        # Construct run parameters query
        paramQuery <- paste("INSERT INTO `run_parameters` (",
            paste(names(runParameters),collapse=","),") VALUES (",
            paste(runParameters,collapse=","),")",sep="")
        
        # Construct delete run parameters query
        paramQueryRm <- paste("DELETE from run_parameters WHERE ref_run_id = '",
            pipelineInput$currentRunId,"'",sep = "")

        # Construct run info query
        infoQuery <- paste("INSERT INTO `run_info` (",
            paste(names(runInfo),collapse=","),") VALUES (",
            paste(runInfo,collapse=","),")",sep="")
        
        # Construct delete run info query
        infoQueryRm <- paste("DELETE from run_info WHERE run_id = '",
            pipelineInput$currentRunId,"'",sep = "")
            
        # Booleans to control what has been written
        paramWritten <- infoWritten <- FALSE
        
        # Keep a log
        pipelineInput$dbWriteLogFile <- 
            file.path(pipelineInput$scriptPath,"dbwrite.Rout")
        dbLog <- file(pipelineInput$dbWriteLogFile,open="wt")
        
        sink(dbLog)
        sink(dbLog,type="message")
        
        # Open connection
        tryCatch({
            con <- dbConnect(SQLite(),dbname=APP_DB)
            
            # Write data
            tryCatch({
                message("Sending query to write run parameters:")
                message(paramQuery)
                nr <- dbExecute(con,paramQuery)
                message(nr, "rows inserted")
                paramWritten <- TRUE
            },error=function(e) {
                message("Caught error while writing run parameters:")
                message(e)
                paramWritten <- FALSE
            },finally="")
            
            tryCatch({
                message("Sending query to write run info:")
                message(infoQuery)
                nr <- dbExecute(con,infoQuery)
                message(nr, "rows inserted")
                infoWritten <- TRUE
            },error=function(e) {
                message("Caught error while writing run info:")
                message(e)
                infoWritten <- FALSE
            },finally="")
            
            # Disconnect to remove lock
            dbDisconnect(con)
            
            # Update buttons etc.
            updateActionButton(session,"saveAnalysis",label="Analysis saved!",
                icon=icon("thumbs-o-up"))
            updateActionButton(session,"discardAnalysis",
                label="Start new analysis",icon=icon("star"))
            shinyjs::disable("saveAnalysis")
            pipelineControl$analysisSaved <- TRUE
        },error=function(e) {
            message("Caught error: ",e)
            message("Clearing potential leftovers")
            con <- dbConnect(SQLite(),dbname=APP_DB)
            if (!paramWritten) {
                nrr <- dbExecute(con,paramQueryRm)
                message(nrr," parameter results cleared")
            }
            if (!infoWritten) {
                nrr <- dbExecute(con,infoQueryRm)
                message(nrr," info results cleared")
            }
            dbDisconnect(con)
            
            # Update buttons etc.
            updateActionButton(session,"saveAnalysis",label="Save failed!",
                icon=icon("thumbs-o-down"))
            updateActionButton(session,"discardAnalysis",
                label="Retry analysis",icon=icon("star-half-o"))
            shinyjs::disable("saveAnalysis")
            pipelineControl$uiError <- TRUE
        },finally="")
        
        # Delete data directory
        message("Deleting CDF file data directory ",pipelineInput$dataPath)
        tryCatch({
			ex <- unlink(pipelineInput$dataPath,recursive=TRUE,force=TRUE)
			message("unlink command exit was ",ex)
		},error=function(e) {
			message("Caught error while deleting CDF data directory ",e)
			message("unlink command exit was ",ex)
		},finally="")
		
        sink(type="message")
        sink()
        close(dbLog)
    })
    
    getDiagTab <- eventReactive(input$analysisDiagnosticPlots,{
        if (pipelineControl$step == "result") {
            val <- input$analysisDiagnosticPlots
            if (!is.null(val)) {
                pipelineResults$currentIndex <- 
                    as.numeric(strsplit(val,"_")[[1]][2])
                updateTabsetPanel(session,"analysisDiagnosticPlots",
                    selected=val)
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
        preDiscardAnalysis=preDiscardAnalysis,
        discardAnalysis=discardAnalysis,
        newAnalysis=newAnalysis,
        saveAnalysis=saveAnalysis,
        getDiagTab=getDiagTab
    ))
}

analysisTabPanelReactive <- function(input,output,session,
    allReactiveVars) {
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
			# *&*@^#&%^###!!@@
			isolate({
				pipelineInput$uploadedFiles <- input$projectFiles$datapath
				pipelineInput$filenames <- input$projectFiles$name
				pipelineControl$filesUploaded <- TRUE
				lapply(1:length(input$projectFiles$name),function(i) {
					#updateTextInput(session,paste("sampleName_",i,sep=""),value="")
					updateTextInput(session,paste("className_",i,sep=""),value="")
				})
			})
        }
    })
    
    # Uploaded files
    uploadSampleFile <- reactive({
        if (!is.null(input$sampleInfoFile)) {
            meta <- read.delim(input$sampleInfoFile$datapath)
            pipelineInput$filenames <- as.character(meta[,1])
            pipelineInput$classes <- as.character(meta[,2])
        }
    })
    
    # Sample classes
    classNames <- reactive({
        if (pipelineControl$step=="preprocess") {
			pipelineInput$classes <- 
				sapply(1:length(pipelineInput$filenames),function(i) {
					n <- input[[paste("className_",i,sep="")]]
					if (length(n) > 0)
						return(n)
					return("")
				})
        }
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
    
    handleExportResultsDownload <- reactive({
        output$exportResults <- downloadHandler(
            filename=function() {
                tt <- paste(pipelineInput$currentRunId,"_",
                    format(Sys.time(),format="%Y%m%d%H%M%S"),".txt",sep="")
            },
            content=function(con) {
                peaks <- pipelineResults$peaks
                norm <- pipelineResults$norm$norm
                exportType <- input$export
                
                metaData <- tryCatch(attr(peaks,"meta.data"),
                    error=function(e) { 
                        return(NULL) 
                },finally="")
                
                if (!is.list(peaks)) {
                    p <- list()
                    p[[1]] <- peaks
                    if (is.null(metaData))
                        names(p) <- "peakdata"
                    else
                        names(p) <- metaData$Samplename
                    peaks <- p
                }
                
                if (!is.null(metaData))
                    expnames <- metaData$Replicate
                else {
                    if (!is.null(names(peaks)))
                        expnames <- names(peaks)
                    else
                        expnames <- paste("Sample",1:length(peaks))
                }
                
                normRef <- as.data.frame(norm$reference)
                normMz <- as.data.frame(norm$mz)
                normRt <- as.data.frame(norm$rt)
                normInten <- as.data.frame(norm$norminten)
                names(normMz) <- paste("mz - ",expnames)
                names(normRt) <- paste("rt - ",expnames)
                names(normInten) <- paste("Intensity - ",expnames)

                if (exportType=="all") {
                    final <- cbind(normRef,normMz,normRt,normInten)
                    write.table(final,file=con,quote=FALSE,sep="\t",na="-",
                        row.names=FALSE)
                }
                else if (exportType=="armada") {
                    tmpNorm <- norm$norminten
                    tmpNorm[which(tmpNorm==0)] <- "NaN"
                    tmpNorm <- as.data.frame(tmpNorm)
                    names(tmpNorm) <- paste("Intensity - ",expnames)
                    final <- cbind(normRef[,"id"],tmpNorm)
                    nam <- names(final)
                    nam[1] <- "id"
                    names(final) <- nam
                    write.table(final,file=con,quote=FALSE,sep="\t",na="NaN",
                        row.names=FALSE)
                }
            }
        )
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
        uploadSampleFile=uploadSampleFile,
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
        finalStdint=finalStdint,
        handleExportResultsDownload=handleExportResultsDownload
    ))
}

analysisTabPanelRenderUI <- function(output,session,allReactiveVars) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    pipelineResults <- allReactiveVars$pipelineResults
    timeFilter <- allReactiveVars$timeFilter
    
    # File input
    output$projectFilesWrapper <- renderUI({
		if (pipelineControl$firstRun || pipelineControl$newAnalysis) {
			fluidRow(column(12,
				fileInput(
					inputId="projectFiles", 
					label="Upload NetCDF files",
					multiple=TRUE,
					accept=c(
						"application/x-netcdf",
						"application/x-netcdf4"
					)
				)
			))
		}
		else
			fluidRow(column(12,""))
	})
	
	# Class file input
	output$sampleInfoFileWrapper <- renderUI({
		if (pipelineControl$firstRun || pipelineControl$newAnalysis) {
			fluidRow(column(12,					
				fileInput(
				    inputId="sampleInfoFile", 
				    label="Upload sample-class relationship file",
				    accept=c("text/*")
				)
			))
		}
	})
	
	# Filenames and classes
    output$sampleInfoEdit <- renderUI({
        if ((pipelineControl$firstRun || pipelineControl$newAnalysis)
			&& !is.null(pipelineInput$filenames)) {
			lapply(1:length(pipelineInput$filenames),function(i,n,m) {
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
                        value=tryCatch(m[i],error=function(e) {
                            return("Enter class name")
                        },finally=""),
                        label=""
                    )
                ))
            },pipelineInput$filenames,pipelineInput$classes)
        }
    })
    
    # Sample info table
    output$sampleInfoTable <- renderTable({
        if ((pipelineControl$firstRun || pipelineControl$newAnalysis)
			&& !is.null(pipelineInput$filenames)
            && !any(is.null(pipelineInput$classes))
            && all(length(pipelineInput$classes)>0)
            && length(pipelineInput$filenames)==length(pipelineInput$classes)) {
            data.frame(
                Filename=pipelineInput$filenames,
                Class=pipelineInput$classes
            )
        }
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
                                ERROR_MESSAGES$filterTimeMin)
                        ),column(2,
                            disabled(numericInput(
                                inputId=paste("reviewMaxTime",i,sep="_"),
                                label="Review max time (seconds)", 
                                value=timeFilter$max,
                                min=0
                            )),
                            div(id=paste("filterTimeMaxError",i,sep="_"),
                                class="input-error",
                                ERROR_MESSAGES$filterTimeMax)
                        ),column(6," "
                        ),
                        div(
                            id=paste("filterTimeCompError",i,sep="_"),
                            class="input-error",
                            ERROR_MESSAGES$filterTimeComparison
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
                            "be reviewed in the tabs below. You can also ",
                            "choose one of the following actions. Discard ",
                            "analysis will get you to the new analysis page."
                        )
                    )),
                    fluidRow(br()),
                    fluidRow(column(6,
                        div(
                            class="pull-left",
                            downloadButton(
                                outputId="exportResults",
                                label="Download results",
                                class="btn-black"
                            )
                        )
                    ),column(3,
                        div(
                            class="pull-left",
                            actionButton(
                                inputId="discardAnalysis",
                                label="Discard analysis",
                                icon=icon("ban")
                            )
                        )
                    ),column(3,
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
                    fluidRow(column(12,
                        div(id="analysisWriteError",class="input-error",
                            ERROR_MESSAGES$analysisWriteError)
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

analysisTabPanelObserve <- function(input,output,session,allReactiveVars) {
    pipelineControl <- allReactiveVars$pipelineControl
    pipelineInput <- allReactiveVars$pipelineInput
    pipelineResults <- allReactiveVars$pipelineResults
    
    # Initialize observing reactive events
    analysisTabPanelReactiveEvents <- 
        analysisTabPanelEventReactive(input,output,session,
            allReactiveVars)
    
    runPreprocess <- analysisTabPanelReactiveEvents$runPreprocess
    resetPreprocess <- analysisTabPanelReactiveEvents$resetPreprocess
    resetToBack <- analysisTabPanelReactiveEvents$resetToBack
    resetNormalization <- analysisTabPanelReactiveEvents$resetNormalization
    resetTimeBoundaries <- analysisTabPanelReactiveEvents$resetTimeBoundaries
    proceedToNormalization <- 
        analysisTabPanelReactiveEvents$proceedToNormalization
    runNormalization <- analysisTabPanelReactiveEvents$runNormalization
    preDiscardAnalysis <- analysisTabPanelReactiveEvents$preDiscardAnalysis
    discardAnalysis <- analysisTabPanelReactiveEvents$discardAnalysis
    newAnalysis <- analysisTabPanelReactiveEvents$newAnalysis
    saveAnalysis <- analysisTabPanelReactiveEvents$saveAnalysis
    getDiagTab <- analysisTabPanelReactiveEvents$getDiagTab

    # Initialize observing reactive expressions
    analysisTabPanelReactiveExprs <- 
        analysisTabPanelReactive(input,output,session,allReactiveVars)
    
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
    uploadSampleFile <- analysisTabPanelReactiveExprs$uploadSampleFile
    classNames <- analysisTabPanelReactiveExprs$classNames
    spectralReviewPlots <- analysisTabPanelReactiveExprs$spectralReviewPlots
    doTimeFilterReview <- analysisTabPanelReactiveExprs$doTimeFilterReview
    finalAlignmentPlots <- analysisTabPanelReactiveExprs$finalAlignmentPlots
    finalDeviationPlots <- analysisTabPanelReactiveExprs$finalDeviationPlots
    finalBoxplots <- analysisTabPanelReactiveExprs$finalBoxplots
    finalRawint <- analysisTabPanelReactiveExprs$finalRawint
    finalNormint <- analysisTabPanelReactiveExprs$finalNormint
    finalStdint <- analysisTabPanelReactiveExprs$finalStdint
    handleExportResultsDownload <- 
        analysisTabPanelReactiveExprs$handleExportResultsDownload
    
    # Initialize UI element reactivity  
    analysisTabPanelRenderUI(output,session,allReactiveVars)
    
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
            print(e)
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
        handleExportResultsDownload()
    })
    observe({
        saveAnalysis()
    })
    observe({
        preDiscardAnalysis()
    })
    observe({
        discardAnalysis()
    })
    observe({
        newAnalysis()
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
