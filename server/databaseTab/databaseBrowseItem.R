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
    
    output$browseReferenceMetabolites <- renderDT({
        con <- dbConnect(drv=RSQLite::SQLite(),dbname=METABO_DB)
        metaboDB <- dbGetQuery(con,DB_QUERIES$METAB_ALL_INFO)
        dbDisconnect(con)
        datatable(
            metaboDB,
            rownames=FALSE,
            class="display",
            filter="top",
            options=list(
                scrollX=TRUE
            ),
            #extensions='Buttons',
            #buttons=I('colvis'),
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
