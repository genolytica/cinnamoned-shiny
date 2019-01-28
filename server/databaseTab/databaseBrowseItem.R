databaseBrowseTabPanelEventReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
		
	return(list())
}

databaseBrowseTabPanelReactive <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	
	return(list())
}

databaseBrowseTabPanelRenderUI <- function(output,session,allReactiveVars,
    allReactiveMsgs) {
	
	output$browseReferenceMetabolites <- renderUI({
		con <- dbConnect(drv=RSQLite::SQLite(),dbname=METABO_DB)
		metaboDB <- dbGetQuery(con,DB_QUERIES$METAB_ALL_INFO)
		dbDisconnect(con)
		#names(metaboDB) <- c("Run ID", "Project Name", "Date")
		#cinnamonDB$Date <- as.POSIXlt(cinnamonDB$Date, format = "%Y-%m-%d %H:%M:%S")
		output$browseReferenceMetabolites <- renderDT(
			metaboDB,
			rownames=FALSE,
			class="display",
			filter="top",
			selection=list(
				mode="single",
				target="cell"
			)
		)
	})
}

databaseBrowseTabPanelObserve <- function(input,output,session,
    allReactiveVars,allReactiveMsgs) {
	databaseBrowseTabPanelReactiveEvents <- 
        databaseBrowseTabPanelEventReactive(input,output,session,
            allReactiveVars,allReactiveMsgs)
            
    databaseBrowseTabPanelReactiveExprs <- 
        databaseBrowseTabPanelReactive(input,output,session,allReactiveVars,
            allReactiveMsgs)
            
    databaseBrowseTabPanelRenderUI(output,session,allReactiveVars,
		allReactiveMsgs)
}
