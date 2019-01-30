databaseSearchTabPanelEventReactive <- function(input,output,session,
    allReactiveVars) {
}

databaseSearchTabPanelReactive <- function(input,output,session,
    allReactiveVars) {
    updateMetaboIds <- reactive({
        m <- isolate({input$metaboDbId})
        con <- dbConnect(drv=RSQLite::SQLite(),dbname=METABO_DB)
        metabs <- dbGetQuery(con,paste(DB_QUERIES$AUTO_METAB_1,' \'%',
            m,'%\' ',DB_QUERIES$AUTO_METAB_2,sep=""))
        dbDisconnect(con)
        if (nrow(metabs) > 0) {
            updateSelectizeInput(session,"metaboDbId",
                choices=as.character(metabs[,1]),
                selected=m,
                server=TRUE
            )
        }
    })
    
    checkComplyButton <- reactive({
        if (input$metaboliteFilters=='dbId') {
            if (!is.null(input$metaboDbId) && input$metaboDbId != "")
                shinyjs::enable("fetchMetabolites")
            else
                shinyjs::disable("fetchMetabolites")
        }
    })
    
    validateMzRangeFrom <- reactive({
        mzRangeFrom <- as.numeric(input$lowerLimit)
        if (mzRangeFrom < 0 || is.na(mzRangeFrom)) {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    })
    
    validateMzRangeTo <- reactive({
        mzRangeTo <- as.numeric(input$upperLimit)
        if (mzRangeTo < 0 || is.na(mzRangeTo)) {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    })
    
    validateDbId <- reactive({
        id <- input$metaboFiltersDbID
        if (id == "") {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    })
    
    return(list(
        validateMzRangeFrom=validateMzRangeFrom,
        validateMzRangeTo=validateMzRangeTo,
        validateDbId=validateDbId,
        updateMetaboIds=updateMetaboIds,
        checkComplyButton=checkComplyButton
    ))
}

databaseSearchTabPanelRenderUI <- function(output,session,allReactiveVars) {
    output$metaboSummary = renderDT({
        canFetch <- FALSE
        if (input$metaboliteFilters=='dbId') {
            if (!is.null(input$metaboDbId) && input$metaboDbId != "") {
                m <- input$metaboDbId
                mq <- paste(paste("'",m,"'",sep=""),collapse=",")
                query <- paste(DB_QUERIES$METAB_BY_ID_1," (",mq," ) ",
                    DB_QUERIES$METAB_BY_ID_2,sep="")
                canFetch <- TRUE
            }
        }
        else if (input$metaboliteFilters=='mzRange') {
            mzMin <- as.numeric(input$lowerLimit)
            mzMax <- as.numeric(input$upperLimit)
            if (!is.na(mzMin) && !is.na(mzMax) && mzMin > 0 && mzMax > 0) {
                query <- paste(DB_QUERIES$METAB_BY_RANGE_1,mzMin,
                    DB_QUERIES$METAB_BY_RANGE_2,mzMax,
                    DB_QUERIES$METAB_BY_RANGE_3)
                canFetch <- TRUE
            }
        }
        
        if (canFetch) {
            con <- dbConnect(drv=RSQLite::SQLite(),dbname=METABO_DB)
            metaboTable <- dbGetQuery(con,query)
            dbDisconnect(con)
            
            datatable(metaboTable,
                rownames=FALSE,
                class="display",
                filter="none",
                escape=FALSE,
                selection=list(
                    mode="single",
                    target = 'cell'
                )
            ) %>% formatStyle(1,cursor='alias')
        }        
    })
}

databaseSearchTabPanelObserve <- function(input,output,session,
    allReactiveVars) {
        
  databaseSearchTabPanelReactiveExprs <- 
    databaseSearchTabPanelReactive(input,output,session,allReactiveVars)
        
    validateMzRangeFrom <- 
        databaseSearchTabPanelReactiveExprs$validateMzRangeFrom
    validateMzRangeTo <- 
        databaseSearchTabPanelReactiveExprs$validateMzRangeTo
    validateHMDBid <- 
        databaseSearchTabPanelReactiveExprs$validateDbId
    updateMetaboIds <- databaseSearchTabPanelReactiveExprs$updateMetaboIds
    checkComplyButton <- 
        databaseSearchTabPanelReactiveExprs$checkComplyButton
        
    databaseSearchTabPanelRenderUI(output,session,allReactiveVars)
    
    observe({
        updateMetaboIds()
    })
    observe({
        checkComplyButton()
    })
 
    observe({
        if (validateMzRangeTo() && validateMzRangeFrom())
            shinyjs::enable("fetchMetabolites")
        else
            shinyjs::disable("fetchMetabolites")
    })
}
