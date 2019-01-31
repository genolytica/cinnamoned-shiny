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
            
            metaboTable$is <- ifelse(
				metaboTable$is_both,"both",ifelse(
					metaboTable$is_geom & !metaboTable$is_rlm,"geom",ifelse(
						!metaboTable$is_geom && metaboTable$is_rlm,"rlm","no"
					)
				)
			)
            
            datatable(metaboTable[,c("id","mz","rt","is")],
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
    
    output$metaboDetails <- renderUI({
        info <- input$metaboSummary_cell_clicked
        if (is.null(info$value) || info$col != 0) 
            return(HTML('Click on a Run ID'))
        
        selectedID = info$value
        con <- dbConnect(drv=RSQLite::SQLite(),dbname=METABO_DB)
        metaboDet <- dbGetQuery(con,paste(DB_QUERIES$METAB_INFO,"'",selectedID,
			"'",sep=""))
        dbDisconnect(con)
    
        metaboDet$isotopes <- ifelse(metaboDet$isotopes=="","-",
			metaboDet$isotopes)
        metaboDet$adduct <- ifelse(metaboDet$adduct=="","-",metaboDet$adduct)
        metaboDet$real_mass <- ifelse(metaboDet$real_mass==0,"-",
			metaboDet$real_mass)
        metaboDet$prop_formula <- ifelse(metaboDet$prop_formula=="","-",
			metaboDet$prop_formula)
        metaboDet$theor_mass <- ifelse(metaboDet$theor_mass=="","-",
			metaboDet$theor_mass)
        metaboDet$is_geom <- ifelse(metaboDet$is_geom==0,"No","Yes")
        metaboDet$is_rlm <- ifelse(metaboDet$is_rlm==0,"No","Yes")
        metaboDet$is_both <- ifelse(metaboDet$is_both==0,"No","Yes")
        
        fluidRow(column(12,
			fluidRow(column(6,
				wellPanel(
					h3("Spectrum"),
					hr(),
					fluidRow(column(6,
						"Peak ID"
					),column(6,
						tags$strong(metaboDet$id)
					)),
					fluidRow(column(6,
						"m/z"
					),column(6,
						tags$strong(metaboDet$mz)
					)),
					fluidRow(column(6,
						"Retention time"
					),column(6,
						tags$strong(metaboDet$rt)
					)),
					fluidRow(column(6,
						"Peak m/z minimum"
					),column(6,
						tags$strong(metaboDet$mzmin)
					)),
					fluidRow(column(6,
						"Peak m/z maximum"
					),column(6,
						tags$strong(metaboDet$mzmax)
					)),
					fluidRow(column(6,
						"Peak RT minimum"
					),column(6,
						tags$strong(metaboDet$rtmin)
					)),
					fluidRow(column(6,
						"Peak RT maximum"
					),column(6,
						tags$strong(metaboDet$rtmax)
					)),
					class="well-panel"
				)
			),column(6,
				wellPanel(
					h3("Deconvolution"),
					hr(),
					fluidRow(column(6,
						"Isotope"
					),column(6,
						tags$strong(metaboDet$isotopes)
					)),
					fluidRow(column(6,
						"Adduct"
					),column(6,
						tags$strong(metaboDet$adduct)
					)),
					fluidRow(column(6,
						"Real mass"
					),column(6,
						tags$strong(metaboDet$real_mass)
					)),
					fluidRow(column(6,
						"Proposed formula"
					),column(6,
						tags$strong(metaboDet$prop_formula)
					)),
					fluidRow(column(6,
						"Theoretical mass"
					),column(6,
						tags$strong(metaboDet$theor_mass)
					)),
					class="well-panel"
				)
			)),
			fluidRow(column(6,
				wellPanel(
					h3("Intensity"),
					hr(),
					fluidRow(column(6,
						"Geometrical IS normalized"
					),column(6,
						tags$strong(metaboDet$summarized_intensity_geom)
					)),
					fluidRow(column(6,
						"RLM IS normalized"
					),column(6,
						tags$strong(metaboDet$summarized_intensity_rlm)
					)),
					fluidRow(column(6,
						"In both IS normalized"
					),column(6,
						tags$strong(metaboDet$summarized_intensity_both)
					)),
					class="well-panel"
				)
			),column(6,
				wellPanel(
					h3("Stability"),
					hr(),
					fluidRow(column(6,
						"Geometrical"
					),column(6,
						tags$strong(metaboDet$is_geom)
					)),
					fluidRow(column(6,
						"RLM"
					),column(6,
						tags$strong(metaboDet$is_rlm)
					)),
					fluidRow(column(6,
						"Both"
					),column(6,
						tags$strong(metaboDet$is_both)
					)),
					class="well-panel"
				)
			))
		))
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
