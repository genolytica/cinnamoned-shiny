databaseTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
}

databaseTabPanelReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	
	filter <- reactive({input$metaboliteFilters})
	
	metaboHmdbidFilter <- reactive({
		if (input$metaboliteFilters=='hmdbID') {
			filter=input$metaboFiltersHmdbID
		}
	})
	
	validateMZRangeFrom <- reactive({
        mzrangefrom <- as.numeric(input$lowerLimit)
        if (mzrangefrom < 0 || is.na(mzrangefrom)) {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    })
	
    validateMZRangeTo <- reactive({
        mzrangeto <- as.numeric(input$upperLimit)
        if (mzrangeto < 0 || is.na(mzrangeto)) {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    })
    
    validateHMDBid <- reactive({
        hmdbid <- input$metaboFiltersHmdbID
        if (hmdbid == "") {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    })
    return(list(
        validateMZRangeFrom=validateMZRangeFrom,
        validateMZRangeTo=validateMZRangeTo,
        validateHMDBid=validateHMDBid,
        filter=filter
    ))
}

databaseTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
}

databaseTabPanelObserve <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
  metaboFilter <- allReactiveVars$metaboFilter
  
  con <- dbConnect(drv=RSQLite::SQLite(),dbname=APP_DB)
  cinnamonDB <- dbGetQuery(con, "SELECT run_id,project_name,date FROM run_info ORDER BY date")
  names(cinnamonDB) <- c("Run ID", "Project Name", "Date")
  cinnamonDB$Date <- as.POSIXlt(cinnamonDB$Date, format = "%Y-%m-%d %H:%M:%S")
  runInfoTable <- datatable(cinnamonDB, 
                            rownames = FALSE,
                            filter = 'bottom',
                            class = 'cell-border strip hover',
                            selection = list(
                              mode="single",
                              target = 'cell')
                            ) %>% formatStyle(1, cursor = 'alias')
  
  output$runInfo = DT::renderDataTable({
    runInfoTable
  }, escape = FALSE)
  
  output$paramsInfo = renderUI({
    info = input$runInfo_cell_clicked
    if (is.null(info$value) || info$col != 0) return(HTML('Click on a Run ID'))
    selectedID = info$value

    runDetails<-dbGetQuery(con, paste0("SELECT ref_run_id,
                              xcms_filter_do,
                              xcms_filter_min,
                              xcms_filter_max,
                              xcms_read_profstep,
                              xcms_read_profmethod,
                              xcms_find_snthresh,
                              xcms_find_step,
                              xcms_find_fwhm,
                              xcms_find_sigma,
                              xcms_find_steps,
                              xcms_find_max,
                              xcms_find_mzdiff,

                              norm_method,
                              norm_tol,
                              norm_correctfor,
                              norm_export,
                              norm_diagplot,
                              norm_tspan,
                              norm_tit,
                              norm_corrfac,
                              norm_cutq,
                              norm_normalize,
                              norm_ispan,
                              norm_cutrat,
                              norm_times

                              FROM run_parameters 
                              WHERE ref_run_id = '", selectedID,"'"))
    
    runDetails$xcms_filter_do <- gsub("1", "Yes", runDetails$xcms_filter_do)
    runDetails$xcms_filter_do <- gsub("0", "No", runDetails$xcms_filter_do)
    
    runDetails$norm_diagplot <- gsub("1", "Yes", runDetails$norm_diagplot)
    runDetails$norm_diagplot <- gsub("0", "No", runDetails$norm_diagplot)
    

    categoryTitle1    <- "<mark>General</mark>"
    analysisID        <- (paste0('<b>Analysis ID: </b>',runDetails$ref_run_id,'\n'))
    InputRetTimeTrunc <- (paste0('<b>Input retention time truncation: </b>',runDetails$xcms_filter_do,'\n'))
    lowerTruncBound   <- (paste0('<b>Lower truncation boundary: </b>',runDetails$xcms_filter_min,'\n'))
    upperTruncBound   <- (paste0('<b>Upper truncation boundary: </b>',runDetails$xcms_filter_max,'\n'))
    categoryTitle2    <- "<mark>Peak Detection</mark>"
    profGenStep       <- (paste0('<b>Profile generation step: </b>',runDetails$xcms_read_profstep,'\n'))
    profGenMethod     <- (paste0('<b>Profile generation method: </b>',runDetails$xcms_read_profmethod,'\n'))
    StoNthreshold     <- (paste0('<b>Signal to noise threshold: </b>',runDetails$xcms_find_snthresh,'\n'))
    peakDetectionStep <- (paste0('<b>Peak detection step: </b>',runDetails$xcms_find_step,'\n'))
    peakFWHM          <- (paste0('<b>Peak full width at half maximum: </b>',runDetails$xcms_find_fwhm,'\n'))
    peakSigma         <- (paste0('<b>Peak model standard deviation: </b>',runDetails$xcms_find_sigma,'\n'))
    findSteps         <- (paste0('<b>EIBPC combine steps: </b>',runDetails$xcms_find_steps,'\n'))
    maxPeaks          <- (paste0('<b>Maximum peaks per EIBPC: </b>',runDetails$xcms_find_max,'\n'))
    findMZdiff        <- (paste0('<b>Minimum m/z difference for overlapping peaks: </b>',runDetails$xcms_find_mzdiff,'\n'))
    categoryTitle3    <- "<mark>Normalization</mark>"
    normMethod        <- (paste0('<b>Standards selection method: </b>',runDetails$norm_method,'\n'))
    normTol           <- (paste0('<b>Reference match m/z tolerance: </b>',runDetails$norm_tol,'\n'))
    normCorrectFor    <- (paste0('<b>Corrected elements requested: </b>',runDetails$norm_correctfor,'\n'))
    normExport        <- (paste0('<b>Export results type: </b>',runDetails$norm_export,'\n'))
    normDiagPlot      <- (paste0('<b>Diagnostic plots requested: </b>',runDetails$norm_diagplot,'\n'))
    normTspan         <- (paste0('<b>Retention time alignment LOESS span: </b>',runDetails$norm_tspan,'\n'))
    normTit           <- (paste0('<b>Retention time iterations: </b>',runDetails$norm_tit,'\n'))
    normCorrFac       <- (paste0('<b>Retention time alignment LOESS correction factor: </b>',runDetails$norm_corrfac,'\n'))
    normCutQ          <- (paste0('<b>Retention time alignment exclusion quantile: </b>',runDetails$norm_cutq,'\n'))
    normNormalize     <- (paste0('<b>Intensity normalization method: </b>',runDetails$norm_normalize,'\n'))
    normIntSpan       <- (paste0('<b>Intensity normalization LOESS span: </b>',runDetails$norm_ispan,'\n'))
    normCutRat        <- (paste0('<b>Non-standards filter threshold: </b>',runDetails$norm_cutrat,'\n'))
    normTimes         <- (paste0('<b>Manual time filters: </b>',runDetails$norm_times,'\n'))
    categoryTitle4    <- "<mark>Analysis</mark>"
    
    HTML(paste(categoryTitle1, analysisID, InputRetTimeTrunc, lowerTruncBound, upperTruncBound, categoryTitle2, profGenStep,
               profGenMethod, StoNthreshold, peakDetectionStep, peakFWHM, peakSigma, findSteps, maxPeaks, findMZdiff,
               categoryTitle3, normMethod, normTol, normCorrectFor, normExport, normDiagPlot, normTspan, normTit, normCorrFac,
               normCutQ, normNormalize, normIntSpan, normCutRat, normTimes, categoryTitle4, sep="<br/>"))
  })
  
  # Initialize observing reactive expressions
  databaseTabPanelReactiveExprs <- 
  	databaseTabPanelReactive(input,output,session,allReactiveVars,
    	allReactiveMsgs)
        
    validateMZRangeFrom <- 
        databaseTabPanelReactiveExprs$validateMZRangeFrom
    validateMZRangeTo <- 
        databaseTabPanelReactiveExprs$validateMZRangeTo
    validateHMDBid <- 
        databaseTabPanelReactiveExprs$validateHMDBid
    filter <- 
        databaseTabPanelReactiveExprs$filter

   # Set the observers
   # Validators
  	observe({
    	if (validateHMDBid())
      		shinyjs::enable("calculateMetaboFilter")
    	else
    		shinyjs::disable("calculateMetaboFilter")
  		output$filter <- renderText(
  		if (filter() == 'hmdbID'){
  			#Metabolite ID QUERY HERE 
  			return(input$metaboFiltersHmdbID)
  		}
  	)
  	})
 
  	observe({
    	if (validateMZRangeTo() && validateMZRangeFrom())
      		shinyjs::enable("calculateMetaboFilter")
    	else
    		shinyjs::disable("calculateMetaboFilter")
  		  	 output$filter <- renderText(
  		if (filter() == 'mzRange'){
  			#m/z range QUERY HERE
  			return(paste(input$lowerLimit, input$upperLimit))
  		}
  	)
  	})
}
