runViewerTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	
	test <- eventReactive(input$runArchivedAnalysisViewer, {
		renderText({return(input$analysisID)})
  	})
	
	return(list(
		test=test
	))


}


runViewerTabPanelReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	
    finalAlignmentPlots <- reactive({
        # norm <- paste0("/media/HD3/cprocess_tmp/",input$runArchivedAnalysisViewer,"/norm.RData")
        if (!is.null(norm)) {
            pd <- norm$pd
            for (i in 1:length(pd)) {
            	
            	#Where does output[[..]] send stuff?? What's with the double [] ??
            	
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
    
    return(list(
    	finalAlignmentPlots=finalAlignmentPlots
    ))
}

runViewerTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {

	output$spectralTab <- renderUI({
            fluidRow(column(12,
                wellPanel(
                    h4("test"),
                    hr(),
                    do.call(tabsetPanel,c(
                        id="analysisDiagnosticPlots",
                        
                        #need to get the filenames
                        
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
                                value=paste("diagTab",i,sep="_")
                            )
                        	
                        #need to get the filenames
                        	
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

	test <- runViewerTabPanelReactiveEvents$test
	
	# Initialize observing reactive expressions
    runViewerTabPanelReactiveExprs <- 
        runViewerTabPanelReactive(input,output,session,allReactiveVars,
            allReactiveMsgs)
	
    finalAlignmentPlots <- runViewerTabPanelReactiveExprs$finalAlignmentPlots
	
    #DRAFT# user input observer
    observe({
    	selectedID <- input$analysisID
    	if (selectedID == "31082015120830") {
    		shinyjs::enable("runArchivedAnalysisViewer")
    		selectedDir <- paste0("/media/HD3/cprocess_tmp/",selectedID,"/norm.RData")
    		load(selectedDir)
    		output$analysis <- renderText(return(selectedDir))
    	}
    })
    
    observe({
        finalAlignmentPlots()
    })
}
