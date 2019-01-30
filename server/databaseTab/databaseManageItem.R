databaseManageTabPanelEventReactive <- function(input,output,session,
    allReactiveVars) {
    pipelineInput <- allReactiveVars$pipelineInput
    
    delId <- reactiveVal()
    
    preDeleteRun <- eventReactive(input$deleteRunButton, {
        delId(as.numeric(strsplit(input$deleteRunButton, "_")[[1]][2]))
        showModal(modalDialog(
            title="Confirm run delete!",
            "Run ",tags$strong(delId())," is about to be deleted. Are you ",
            "sure? This action cannot be undone!",
            easyClose=FALSE,
            footer=tagList(
                modalButton("Cancel",icon=icon("ban")),
                actionButton("confirmRunDelete","Delete",class="btn-danger",
                    icon=icon("exclamation-triangle"))
            )
        ))
    })
    
    deleteRun <- eventReactive(input$confirmRunDelete,{
        tryCatch({
            con <- dbConnect(drv=RSQLite::SQLite(),dbname=APP_DB)
            rs <- dbSendQuery(con,paste(DB_QUERIES$DELETE_RUN,"'",delId(),"'",
                sep=""))
            dbClearResult(rs)
            dbDisconnect(con)
            unlink(file.path(pipelineInput$basePath,delId(),recursive=TRUE))
            
            showModal(modalDialog(
                title="Run deleted!",
                "Run ",tags$strong(delId())," and all related files have been ",
                    "succesfully deleted!",
                easyClose=TRUE,
                footer=tagList(
                    modalButton("OK",icon=icon("check"))
                )
            ))
        },error=function(e) {
            showModal(modalDialog(
                title="Error!",
                "Run ",tags$strong(delId())," has not been properly deleted! ",
                "Leftovers (files, images) may remain... Please report the ",
                "run ID to the administrator with the following error:",
                tags$br(),e,
                easyClose=FALSE,
                footer=tagList(
                    modalButton("OK",icon=icon("check"))
                )
            ))
        },finally="")
    })
    
    return(list(
        preDeleteRun=preDeleteRun,
        deleteRun=deleteRun
    ))
}

databaseManageTabPanelReactive <- function(input,output,session,
    allReactiveVars) {
}

databaseManageTabPanelRenderUI <- function(output,session,allReactiveVars) {
    output$runInfo = renderDT({
        con <- dbConnect(drv=RSQLite::SQLite(),dbname=APP_DB)
        cinnamonDB <- dbGetQuery(con,DB_QUERIES$RUN_INFO_ALL)
        dbDisconnect(con)
        cinnamonDB$Delete <- shinyInput(actionButton,nrow(cinnamonDB),
            'deleteRun_',cinnamonDB$run_id,label="Delete",
            icon=icon("minus-circle"),class="btn-primary btn-xs",
            onclick='Shiny.onInputChange(\"deleteRunButton\",this.id)')
        names(cinnamonDB)[1:3] <- c("Run ID","Project name","Date")
        cinnamonDB$Date <- as.POSIXct(cinnamonDB$Date,
            format="%Y-%m-%d %H:%M:%S")
        #cinnamonDB$Date$zone <- NULL
        datatable(cinnamonDB,
            rownames=FALSE,
            class="display",
            filter="top",
            escape=FALSE,
            selection=list(
                mode="single",
                target = 'cell'
            ),
            options=list(
                columnDefs=list(
                    list(
                        width="20%",
                        targets=0
                    ),
                    list(
                        width="35%",
                        targets=1
                    ),
                    list(
                        width="30%",
                        targets=2
                    ),
                    list(
                        width="10%",
                        targets=3
                    )
                )
            )
        ) %>% formatStyle(1,cursor='alias') %>% 
            formatDate(3,method='toLocaleString')
    })
    
    output$paramsInfo <- renderUI({
        info <- input$runInfo_cell_clicked
        if (is.null(info$value) || info$col != 0) 
            return(HTML('Click on a Run ID'))
        
        selectedID = info$value
        con <- dbConnect(drv=RSQLite::SQLite(),dbname=APP_DB)
        runDetails <- dbGetQuery(con,paste(DB_QUERIES$INFO_PARAMS,"'",
            selectedID,"'",sep=""))
        dbDisconnect(con)
    
        runDetails$xcms_filter_do <- gsub("1","Yes",runDetails$xcms_filter_do)
        runDetails$xcms_filter_do <- gsub("0","No",runDetails$xcms_filter_do)    
        runDetails$norm_diagplot <- gsub("1","Yes",runDetails$norm_diagplot)
        runDetails$norm_diagplot <- gsub("0","No",runDetails$norm_diagplot)
        
        fluidRow(column(12,
            wellPanel(
                h3("General"),
                hr(),
                fluidRow(column(8,
                    "Analysis ID"
                ),column(4,
                    tags$strong(runDetails$ref_run_id)
                )),
                fluidRow(column(8,
                    "Input retention time truncation"
                ),column(4,
                    tags$strong(runDetails$xcms_filter_do)
                )),
                fluidRow(column(8,
                    "Lower truncation boundary"
                ),column(4,
                    tags$strong(runDetails$xcms_filter_min)
                )),
                fluidRow(column(8,
                    "Upper truncation boundary"
                ),column(4,
                    tags$strong(runDetails$xcms_filter_max)
                )),
                class="well-panel"
            ),
            wellPanel(
                h3("Peak detection"),
                hr(),
                fluidRow(column(8,
                    "Profile generation step"
                ),column(4,
                    tags$strong(runDetails$xcms_read_profstep)
                )),
                fluidRow(column(8,
                    "Profile generation method"
                ),column(4,
                    tags$strong(runDetails$xcms_read_profmethod)
                )),
                fluidRow(column(8,
                    "Signal to noise threshold"
                ),column(4,
                    tags$strong(runDetails$xcms_find_snthresh)
                )),
                fluidRow(column(8,
                    "Peak detection step"
                ),column(4,
                    tags$strong(runDetails$xcms_find_step)
                )),
                fluidRow(column(8,
                    "Peak full width at half maximum"
                ),column(4,
                    tags$strong(runDetails$xcms_find_fwhm)
                )),
                fluidRow(column(8,
                    "Peak model standard deviation"
                ),column(4,
                    tags$strong(runDetails$xcms_find_sigma)
                )),
                fluidRow(column(8,
                    "EIBPC combine steps"
                ),column(4,
                    tags$strong(runDetails$xcms_find_steps)
                )),
                fluidRow(column(8,
                    "Maximum peaks per EIBPC"
                ),column(4,
                    tags$strong(runDetails$xcms_find_max)
                )),
                fluidRow(column(8,
                    "Minimum m/z difference for overlapping peaks"
                ),column(4,
                    tags$strong(runDetails$xcms_find_mzdiff)
                )),
                class="well-panel"
            ),
            wellPanel(
                h3("Normalization"),
                hr(),
                fluidRow(column(8,
                    "Standards selection method"
                ),column(4,
                    tags$strong(runDetails$norm_method)
                )),
                fluidRow(column(8,
                    "Reference match m/z tolerance"
                ),column(4,
                    tags$strong(runDetails$norm_tol)
                )),
                fluidRow(column(8,
                    "Corrected elements requested"
                ),column(4,
                    tags$strong(runDetails$norm_correctfor)
                )),
                fluidRow(column(8,
                    "Export results type"
                ),column(4,
                    tags$strong(runDetails$norm_export)
                )),
                fluidRow(column(8,
                    "Diagnostic plots requested"
                ),column(4,
                    tags$strong(runDetails$norm_diagplot)
                )),
                fluidRow(column(8,
                    "Retention time alignment LOESS span"
                ),column(4,
                    tags$strong(runDetails$norm_tspan)
                )),
                fluidRow(column(8,
                    "Retention time iterations"
                ),column(4,
                    tags$strong(runDetails$norm_tit)
                )),
                fluidRow(column(8,
                    "Retention time alignment LOESS correction factor"
                ),column(4,
                    tags$strong(runDetails$norm_corrfac)
                )),
                fluidRow(column(8,
                    "Retention time alignment exclusion quantile"
                ),column(4,
                    tags$strong(runDetails$norm_cutq)
                )),
                fluidRow(column(8,
                    "Intensity normalization method"
                ),column(4,
                    tags$strong(runDetails$norm_normalize)
                )),
                fluidRow(column(8,
                    "Intensity normalization LOESS span"
                ),column(4,
                    tags$strong(runDetails$norm_ispan)
                )),
                fluidRow(column(8,
                    "Non-standards filter threshold"
                ),column(4,
                    tags$strong(runDetails$norm_cutrat)
                )),
                fluidRow(column(8,
                    "Manual time filters"
                ),column(4,
                    div(
                        class="wordwrap",style="margin-top:-38px;",
                        tags$strong(runDetails$norm_times)
                    )
                )),
                class="well-panel"
            )
        ))
    })
}

databaseManageTabPanelObserve <- function(input,output,session,
    allReactiveVars) {
    
    databaseManageTabPanelReactiveEvents <- 
        databaseManageTabPanelEventReactive(input,output,session,
            allReactiveVars)
    
    preDeleteRun <- databaseManageTabPanelReactiveEvents$preDeleteRun
    deleteRun <- databaseManageTabPanelReactiveEvents$deleteRun
    
    databaseManageTabPanelRenderUI(output,session,allReactiveVars)
  
    observe({
        preDeleteRun()
        deleteRun()
    })
}

# https://stackoverflow.com/questions/45739303/r-shiny-handle-action-buttons-in-data-table
shinyInput <- function(FUN,len,idPrefix,idSuffix,...) {
    inputs <- character(len)
    if (missing(idSuffix))
        idSuffix <- 1:len
    counter <- 0
    for (i in idSuffix) {
        counter <- counter + 1
        inputs[counter] <- as.character(FUN(paste0(idPrefix,i),...))
    }
        
    return(inputs)
}
