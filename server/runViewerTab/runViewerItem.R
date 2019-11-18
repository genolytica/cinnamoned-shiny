runViewerTabPanelEventReactive <- function(input,output,session,
    allReactiveVars) {
    
    pipelineInput <- allReactiveVars$pipelineInput
    runArchive <- allReactiveVars$runArchive
    
    loadSelectedRun <- eventReactive(input$loadSelectedRun,{
        if (!is.null(runArchive$runId)) {
            # Check what exists in the run directory
            runPath <- file.path(pipelineInput$basePath,runArchive$runId)
            runArchive$scriptPath <- file.path(runPath,"scripts")
            peakFile <- file.path(runPath,"peaks.RData")
            normFile <- file.path(runPath,"norm.RData")
            noNormFile <- file.path(runPath,"noNorm.RData")
            tarFile <- file.path(runPath,paste(runArchive$runId,
                ".tar.gz",sep=""))
            if (!dir.exists(runPath) && (!file.exists(peakFile)
                || !file.exists(normFile)) && !file.exists(tarFile)) {
                runArchive$corrupted <- TRUE
                showModal(modalDialog(
                    title="Corrupted run!",
                    "Run ",tags$strong(runArchive$runId)," is probably ",
                    "corrupted! Please report the run ID to the administrator ",
                    "so as to investigate.",
                    easyClose=FALSE,
                    footer=tagList(
                        modalButton("OK",icon=icon("check"))
                    )
                ))
                return()
            }
            
            # 1. Compressed case, old version definitely, uncompress and 
            # continue from there
            if (file.exists(tarFile))
                untar(tarFile,exdir=runPath)
            # 2. Uncompressed case, old version (plot data need recalculation)
            if (file.exists(peakFile)) {
                z <- load(peakFile)
                # peaks variable exists in the environment
                v <- load(normFile)
                # norm variable exist in the environment
                if (file.exists(noNormFile))
					b <- load(noNormFile)
                # norm variable exist in the environment
                
                # The while normalization process must run again...
                if (is.null(norm$pd)) {
                    runArchive$normLogFile <- 
                        file.path(runArchive$scriptPath,"norm.Rout")
                    normLog <- file(runArchive$normLogFile,open="wt")
                    
                    # Retrieve parameteres from database
                    con <- dbConnect(drv=RSQLite::SQLite(),dbname=APP_DB)
                    runInfo <- dbGetQuery(con,
                        paste(DB_QUERIES$NORM_INFO_PARAMS,"'",runArchive$runId,
                            "'",sep=""))
                    dbDisconnect(con)
                    
                    runArchive$normPeaks$method <- 
                        as.character(runInfo$norm_method)
                    runArchive$normPeaks$correctfor <-
                        as.character(runInfo$norm_correctfor)
                    runArchive$normPeaks$mztol <-
                        as.numeric(runInfo$norm_tol)
                    runArchive$normPeaks$export <-
                        as.character(runInfo$norm_export)
                    runArchive$normPeaks$tspan <-
                        as.numeric(runInfo$norm_tspan)
                    runArchive$normPeaks$tit <-
                        as.numeric(runInfo$norm_tit)
                    runArchive$normPeaks$corrfac <-
                        as.numeric(runInfo$norm_corrfac)
                    runArchive$normPeaks$cutq <-
                        as.numeric(runInfo$norm_cutq)
                    runArchive$normPeaks$normalize <-
                        as.character(runInfo$norm_normalize)
                    runArchive$normPeaks$ispan <-
                        as.numeric(runInfo$norm_ispan)
                    runArchive$normPeaks$corrfacNS <-
                        as.numeric(runInfo$norm_cutrat)
                    
                    # Parse refined times
                    if (!is.null(runInfo$norm_times)
                        && runInfo$norm_times != "") {
                        v <- strsplit(as.character(runInfo$norm_times),"),c")
                        if (length(v[[1]]) > 1) {
                            # v[[1]][1] does not have a ")" in the end
                            v[[1]][1] <- paste(v[[1]][1],")",sep="")

                            # The rest up to the last don't have a "c" in the 
                            # beginning and a ")" in the end
                            if (length(v[[1]]) > 2) {
                                for (i in 2:(length(v[[1]])-1))
                                    v[[1]][i] <- paste("c",v[[1]][i],")",sep="")
                            }
                                
                            # The last does not have a "c" in the beginning
                            v[[1]][length(v[[1]])] <- 
                                paste("c",v[[1]][length(v[[1]])],sep="")
                        }
                        
                        runArchive$normPeaks$refinedTimeBoundaries <- 
                            lapply(v[[1]],function(x) eval(parse(text=x)))
                    }
                    
                    # Create the normalization figure output directory if it
                    # does not exist
                    figDir <- file.path(runPath,"diagnostic","normalization")
                    if (!dir.exists(figDir))
                        dir.create(figDir,recursive=TRUE,mode="0755")
                    pipelineInput$diagPathNormalization
                    
                    # Show progress stuff
                    shinyjs::show("progressWrapperA")
                    shinyjs::html("normalizationProgressA",
                        "Normalization running!")
                    
                    sink(normLog)
                    sink(normLog,type="message")
                    
                    noNorm <- normalizeSamples(
                        peaks=peaks,
                        dbdata=METABO_DB,
                        method=runArchive$normPeaks$method,
                        normalize=runArchive$normPeaks$normalize,
                        correctfor="none",
                        time.range=runArchive$normPeaks$refinedTimeBoundaries,
                        tol=runArchive$normPeaks$mztol,
                        tspan=runArchive$normPeaks$tspan,
                        ispan=runArchive$normPeaks$ispan,
                        tit=runArchive$normPeaks$tit,
                        cutq=runArchive$normPeaks$cutq,
                        corrfac=runArchive$normPeaks$corrfac,
                        cutrat=runArchive$normPeaks$input$corrfacNS,
                        export=file.path(runPath,"norm_output.txt"),
                        diagplot=NULL,
                        export.type=as.character(runArchive$normPeaks$export),
                        shinyProgressData=list(
                            session=session,
                            progressId="normalizationProgressBarA",
                            progressTotal=3,
                            textId="normA"
                        )
                    )
                    
                    norm <- normalizeSamples(
                        peaks=peaks,
                        dbdata=METABO_DB,
                        method=runArchive$normPeaks$method,
                        normalize=runArchive$normPeaks$normalize,
                        correctfor=runArchive$normPeaks$correctfor,
                        time.range=runArchive$normPeaks$refinedTimeBoundaries,
                        tol=runArchive$normPeaks$mztol,
                        tspan=runArchive$normPeaks$tspan,
                        ispan=runArchive$normPeaks$ispan,
                        tit=runArchive$normPeaks$tit,
                        cutq=runArchive$normPeaks$cutq,
                        corrfac=runArchive$normPeaks$corrfac,
                        cutrat=runArchive$normPeaks$input$corrfacNS,
                        export=file.path(runPath,"norm_output.txt"),
                        diagplot=figDir,
                        plottype="png",
                        export.type=as.character(runArchive$normPeaks$export),
                        shinyProgressData=list(
                            session=session,
                            progressId="normalizationProgressBarA",
                            progressTotal=3,
                            textId="normA"
                        )
                    )
                    
                    sink(type="message")
                    sink()
                    
                    runArchive$noNormRda <- file.path(runPath,"noNorm.RData")
                    save(noNorm,file=runArchive$noNormRda)
                    runArchive$normRda <- file.path(runPath,"norm.RData")
                    save(norm,file=runArchive$normRda)
                }
                
                # Final assignments
                meta <- attr(peaks,"meta.data")
                runArchive$peaks <- peaks
                runArchive$norm <- norm
                if (file.exists(noNormFile))
					runArchive$noNorm <- noNorm
				else
					runArchive$noNorm <- NULL
                runArchive$filenames <- meta$Filename
                runArchive$classes <- meta$Class
                runArchive$currentIndex <- 1
            }
        }
    })
    
    getDiagTab <- eventReactive(input$runViewerDiagnosticPlots,{
        val <- isolate(input$runViewerDiagnosticPlots)
        if (!is.null(val))
            runArchive$currentIndex <- 
                as.numeric(strsplit(val,"_")[[1]][2])
    })
    
    return(list(
        loadSelectedRun=loadSelectedRun,
        getDiagTab=getDiagTab
    ))
}

runViewerTabPanelReactive <- function(input,output,session,
    allReactiveVars) {
    
    runArchive <- allReactiveVars$runArchive
    
    getCellClick <- reactive({
        info <- input$pastRunInfo_cell_clicked
        if (is.null(info$value) || info$col != 0) {
            runArchive$runId <- NULL
            shinyjs::disable("loadSelectedRun")
            return()
        }
        else {
            runArchive$runId <- info$value
            shinyjs::enable("loadSelectedRun")
        }
    })

    # Final diagnostic plots - alignment
    archiveFinalAlignmentPlots <- reactive({
        norm <- runArchive$norm
        if (!is.null(norm$pd)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("aFinalAlignment",i,sep="_")]] <- renderPlot({
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
    archiveFinalDeviationPlots <- reactive({
        norm <- runArchive$norm
        if (!is.null(norm$pd)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("aFinalDeviation",i,sep="_")]] <- renderPlot({
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
    archiveFinalBoxplots <- reactive({
        norm <- runArchive$norm
        if (!is.null(norm$pd)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("aFinalBoxplot",i,sep="_")]] <- renderPlot({
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
    archiveFinalRawint <- reactive({
        norm <- runArchive$norm
        if (!is.null(norm$pd)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("aFinalRawint",i,sep="_")]] <- renderPlot({
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
    archiveFinalNormint <- reactive({
        norm <- runArchive$norm
        if (!is.null(norm$pd)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("aFinalNormint",i,sep="_")]] <- renderPlot({
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
    archiveFinalStdint <- reactive({
        norm <- runArchive$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output[[paste("aFinalStdint",i,sep="_")]] <- renderPlot({
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
    
    handleExportResultsDownloadA <- reactive({
        output$exportResultsA <- downloadHandler(
            filename=function() {
                tt <- paste(runArchive$runId,"_",
                    format(Sys.time(),format="%Y%m%d%H%M%S"),".txt",sep="")
            },
            content=function(con) {
                peaks <- runArchive$peaks
                norm <- runArchive$norm$norm
                exportType <- runArchive$normPeaks$export
                
                metaData <- tryCatch(attr(peaks,"meta.data"),
                    error=function(e) { 
                        return(NULL) 
                },archiveFinally="")
                
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
                    archiveFinal <- cbind(normRef,normMz,normRt,normInten)
                    write.table(archiveFinal,file=con,quote=FALSE,sep="\t",
                        na="-",row.names=FALSE)
                }
                else if (exportType=="armada") {
                    tmpNorm <- norm$norminten
                    tmpNorm[which(tmpNorm==0)] <- "NaN"
                    tmpNorm <- as.data.frame(tmpNorm)
                    names(tmpNorm) <- paste("Intensity - ",expnames)
                    archiveFinal <- cbind(normRef[,"id"],tmpNorm)
                    nam <- names(archiveFinal)
                    nam[1] <- "id"
                    names(archiveFinal) <- nam
                    write.table(archiveFinal,file=con,quote=FALSE,sep="\t",
                        na="NaN",row.names=FALSE)
                }
            }
        )
    })
    
    handleExportNoNormChooser <- reactive({
		if (!is.null(runArchive$currentIndex) && is.null(runArchive$norm)) {
			showModal(modalDialog(
				title="Non-normalized data unavailable!",
				"Raw intensities for run ",tags$strong(runArchive$runId),
				" is not available as an archive. Please rerun the sample(s) ",
				"with a newer version of cinnamoned.",
				easyClose=FALSE,
				footer=tagList(
					modalButton("OK",icon=icon("check"))
				)
			))
			return()
		}
		else
			handleExportNoNormResultsDownloadA()
	})
    
    handleExportNoNormResultsDownloadA <- reactive({
        output$exportNoNormResultsA <- downloadHandler(
            filename=function() {
                tt <- paste(runArchive$runId,"_NONORM_",
                    format(Sys.time(),format="%Y%m%d%H%M%S"),".txt",sep="")
            },
            content=function(con) {
                peaks <- runArchive$peaks
                norm <- runArchive$noNorm$norm
                exportType <- runArchive$normPeaks$export
                
                metaData <- tryCatch(attr(peaks,"meta.data"),
                    error=function(e) { 
                        return(NULL) 
                },archiveFinally="")
                
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
                    archiveFinal <- cbind(normRef,normMz,normRt,normInten)
                    write.table(archiveFinal,file=con,quote=FALSE,sep="\t",
                        na="-",row.names=FALSE)
                }
                else if (exportType=="armada") {
                    tmpNorm <- norm$norminten
                    tmpNorm[which(tmpNorm==0)] <- "NaN"
                    tmpNorm <- as.data.frame(tmpNorm)
                    names(tmpNorm) <- paste("Intensity - ",expnames)
                    archiveFinal <- cbind(normRef[,"id"],tmpNorm)
                    nam <- names(archiveFinal)
                    nam[1] <- "id"
                    names(archiveFinal) <- nam
                    write.table(archiveFinal,file=con,quote=FALSE,sep="\t",
                        na="NaN",row.names=FALSE)
                }
            }
        )
    })
    
    return(list(
        getCellClick=getCellClick,
        archiveFinalAlignmentPlots=archiveFinalAlignmentPlots,
        archiveFinalDeviationPlots=archiveFinalDeviationPlots,
        archiveFinalBoxplots=archiveFinalBoxplots,
        archiveFinalRawint=archiveFinalRawint,
        archiveFinalNormint=archiveFinalNormint,
        archiveFinalStdint=archiveFinalStdint,
        handleExportResultsDownloadA=handleExportResultsDownloadA,
        #handleExportNoNormResultsDownloadA=handleExportNoNormResultsDownloadA
        handleExportNoNormChooser=handleExportNoNormChooser
    ))
}

runViewerTabPanelRenderUI <- function(output,session,allReactiveVars) {
    runArchive <- allReactiveVars$runArchive
    
    output$pastRunInfo = renderDT({
        con <- dbConnect(drv=RSQLite::SQLite(),dbname=APP_DB)
        cinnamonDB <- dbGetQuery(con,DB_QUERIES$RUN_INFO_ALL)
        dbDisconnect(con)
        names(cinnamonDB) <- c("Run ID","Project name","Date")
        cinnamonDB$Date <- as.POSIXct(cinnamonDB$Date,
            format="%Y-%m-%d %H:%M:%S")
        datatable(cinnamonDB,
            rownames=FALSE,
            class="display",
            filter="top",
            escape=FALSE,
            selection=list(
                mode="single",
                target = 'cell'
            )
        ) %>% formatStyle(1,cursor='alias') %>% 
            formatDate(3,method='toLocaleString')
    })
    
    # Results page
    output$pastRunResults <- renderUI({
        if (!is.null(runArchive$norm)) {
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
                                runArchive$runId
                            ),
                            "has been successfully completed! The results can ",
                            "be reviewed in the tabs below. You can also ",
                            "choose one of the following actions. Discard ",
                            "analysis will get you to the new analysis page."
                        ),
                        fluidRow(br()),
                        fluidRow(column(6,
                            div(
                                class="pull-left",
                                downloadButton(
                                    outputId="exportResultsA",
                                    label="Download normalized results",
                                    class="btn-black"
                                )
                            )
                        ),column(6,
                            div(
                                class="pull-left",
                                downloadButton(
                                    outputId="exportNoNormResultsA",
                                    label="Download raw results",
                                    class="btn-semi-black"
                                )
                            )
                        ))
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
                            runArchive$filenames[
                                runArchive$currentIndex]," - ",
                            runArchive$classes[runArchive$currentIndex]
                        ),
                        div(
                            tags$strong(runArchive$norm$pct[
                                runArchive$currentIndex,"total"]),
                            "% of new m/z matching with reference"
                        ),
                        div(
                            tags$strong(runArchive$norm$pct[
                                runArchive$currentIndex,"is"]),
                            "% of new sample IS matching with reference"
                        ),
                        div(
                            tags$strong(runArchive$norm$pct[
                                runArchive$currentIndex,"is_rt"]),
                            "% of new sample IS used for RT correction"
                        ),
                        div(
                            tags$strong(runArchive$norm$pct[
                                runArchive$currentIndex,"is_inten"]),
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
                        lapply(1:length(runArchive$filenames),function(i,n) {
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
                                        plotOutput(paste("aFinalAlignment",i,
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
                                        plotOutput(paste("aFinalDeviation",i,
                                            sep="_"),height="600px")
                                    ),column(4,
                                        plotOutput(paste("aFinalBoxplot",i,
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
                                        plotOutput(paste("aFinalRawint",i,
                                            sep="_"),height="400px"),
                                        fluidRow(br()),
                                        plotOutput(paste("aFinalNormint",i,
                                            sep="_"),height="400px"),
                                        fluidRow(br()),
                                        plotOutput(paste("aFinalStdint",i,
                                            sep="_"),height="400px")
                                    )),
                                    class="well-panel"
                                ),
                                value=paste("diagTabA",i,sep="_")
                            )
                        },runArchive$filenames)
                    )),
                    class="well-panel"
                )
            ))
            
            ))
        }
    })
}

runViewerTabPanelObserve <- function(input,output,session,allReactiveVars) {
    # Initialize observing reactive events
    runViewerTabPanelReactiveEvents <- 
        runViewerTabPanelEventReactive(input,output,session,allReactiveVars)

    loadSelectedRun <- runViewerTabPanelReactiveEvents$loadSelectedRun
    getDiagTab <- runViewerTabPanelReactiveEvents$getDiagTab
        
    # Initialize observing reactive expressions
    runViewerTabPanelReactiveExprs <- 
        runViewerTabPanelReactive(input,output,session,allReactiveVars)
    
    getCellClick <- runViewerTabPanelReactiveExprs$getCellClick
    archiveFinalAlignmentPlots <- 
        runViewerTabPanelReactiveExprs$archiveFinalAlignmentPlots
    archiveFinalDeviationPlots <- 
        runViewerTabPanelReactiveExprs$archiveFinalDeviationPlots
    archiveFinalBoxplots <- 
        runViewerTabPanelReactiveExprs$archiveFinalBoxplots
    archiveFinalRawint <- 
        runViewerTabPanelReactiveExprs$archiveFinalRawint
    archiveFinalNormint <- 
        runViewerTabPanelReactiveExprs$archiveFinalNormint
    archiveFinalStdint <- 
        runViewerTabPanelReactiveExprs$archiveFinalStdint
    handleExportResultsDownloadA <- 
        runViewerTabPanelReactiveExprs$handleExportResultsDownloadA
    #handleExportNoNormResultsDownloadA <- 
        runViewerTabPanelReactiveExprs$handleExportNoNormResultsDownloadA
    handleExportNoNormChooser <- 
		runViewerTabPanelReactiveExprs$handleExportNoNormChooser
    
    runViewerTabPanelRenderUI(output,session,allReactiveVars)
    
    observe({
        loadSelectedRun()
    })
    
    observe({
        getDiagTab()
    })
    
    observe({
        getCellClick()
    })
    
    observe({
        handleExportResultsDownloadA()
    })
    
    observe({
        #handleExportNoNormResultsDownloadA()
        handleExportNoNormChooser()
    })
    
    observe({
        archiveFinalAlignmentPlots()
        archiveFinalDeviationPlots()
        archiveFinalBoxplots()
        archiveFinalRawint()
        archiveFinalNormint()
        archiveFinalStdint()
    })
}
