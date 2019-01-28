databaseSearchTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
}

databaseSearchTabPanelReactive <- function(input,output,session,
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

databaseSearchTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
}

databaseSearchTabPanelObserve <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
  metaboFilter <- allReactiveVars$metaboFilter
  
  # Initialize observing reactive expressions
  databaseSearchTabPanelReactiveExprs <- 
  	databaseSearchTabPanelReactive(input,output,session,allReactiveVars,
    	allReactiveMsgs)
        
    validateMZRangeFrom <- 
        databaseSearchTabPanelReactiveExprs$validateMZRangeFrom
    validateMZRangeTo <- 
        databaseSearchTabPanelReactiveExprs$validateMZRangeTo
    validateHMDBid <- 
        databaseSearchTabPanelReactiveExprs$validateHMDBid
    filter <- 
        databaseSearchTabPanelReactiveExprs$filter

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
