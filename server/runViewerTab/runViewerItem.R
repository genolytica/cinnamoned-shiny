runViewerTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	
	pipelineResults <- allReactiveVars$pipelineResults
	pipelineInput <- allReactiveVars$pipelineInput
	
	
	runArchivedAnalysisView <- eventReactive(input$runArchivedAnalysisViewer, {
		base <- pipelineInput$basePath
		selectedanalysis <- input$analysisID
		if (selectedanalysis != ""){
		selectedDir <- paste0(base,"/",selectedanalysis,"/")
		normDir <- paste0(selectedDir,"norm.RData")
		load(normDir)
		output$analysis<-renderText({return(selectedDir)})
		output$spectralTab<-renderText({return(paste("Plot will be generated using: ",normDir))})
		}
  	})
	
    getDiagTab <- eventReactive(input$runViewerDiagnosticPlots,{
        val <- isolate(input$runViewerDiagnosticPlots)
        if (!is.null(val))
            pipelineResults$currentIndex <- 
                as.numeric(strsplit(val,"_")[[1]][2])
    })
	
	return(list(
		runArchivedAnalysisView=runArchivedAnalysisView,
		getDiagTab=getDiagTab
	))


}


runViewerTabPanelReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	pipelineResults <- allReactiveVars$pipelineResults

	# Need to manually load user-selected analysis' "norm" so the run Viewer plots can be 
	# generated independently 
	# (e.g without a currently running analysis, or if user goes drectly to runViewr tab)
	#norm<-load(file = "/media/HD3/cprocess_tmp/31082015120830/norm.RData")
	
    finalAlignmentPlots <- reactive({
        norm <- pipelineResults$norm
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
                output$spectralTab <- renderPlot({
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
    
    return(list(
    	finalAlignmentPlots=finalAlignmentPlots
    ))
}

runViewerTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
	
	pipelineInput <- allReactiveVars$pipelineInput

	output$spectralTab <- renderUI({
            fluidRow(column(12,
                wellPanel(
                    h4("testt"),
                    hr(),
                    do.call(tabsetPanel,c(
                        id="runViewerDiagnosticPlots",
                        
                        #need to get the filenames without a currently running analysis
                        
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
                                        plotOutput(spectralTab,height="1200px")
                                    )),
                                    class="well-panel"
                                ),
                                value=paste("diagTab",i,sep="_")
                            )
                        	
                        #need to get the filenames without a currently running analysis
                        	
                        },pipelineInput$filenames)
                    )),
                    class="well-panel"
                )
            ))
	})
}

runViewerTabPanelObserve <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	
	# Initialize observing reactive events
	runViewerTabPanelReactiveEvents <- 
        runViewerTabPanelEventReactive(input,output,session,
            allReactiveVars,allReactiveMsgs)

		runArchivedAnalysisView <- runViewerTabPanelReactiveEvents$runArchivedAnalysisView
		getDiagTab <- runViewerTabPanelReactiveEvents$getDiagTab
		
	# Initialize observing reactive expressions
    runViewerTabPanelReactiveExprs <- 
        runViewerTabPanelReactive(input,output,session,allReactiveVars,
            allReactiveMsgs)
	
    finalAlignmentPlots <- runViewerTabPanelReactiveExprs$finalAlignmentPlots
	
    #DRAFT# user input observer
    observe({
    		shinyjs::enable("runArchivedAnalysisViewer")
    		runArchivedAnalysisView()
    		#load(selectedDir)
    })
    
    observe({
        getDiagTab()
    })
    
    observe({
        finalAlignmentPlots()
    })
}
