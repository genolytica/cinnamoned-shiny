runViewerTabPanel <- function() {
	fluidRow(column(8,h1("Run Output"),
		fluidRow(column(6,
			wellPanel(
      			h4("Type an analysis ID"),
      			hr(),
      			style = "overflow: auto;",
      			class="well-panel",
      			fluidRow(column(10,
                textInput(
                	inputId = "analysisID",
                	label = "",
                	value = "",
                	placeholder = "e.g: 31082015120830"
                )
              	)),
				column(6,
                    div(
                        class="pull-right",
                        style="display:inline-block",
                        disabled(actionButton(
                            inputId="runArchivedAnalysisViewer",
                            label="Engage!",
                            icon=icon("rocket"),
                            class="btn-primary"
                       ))
                    )
                )
			)
		),
      			column(6,
                    wellPanel(
	    				h2("Analysis Directory (TEST)"),
	      				hr(),
	      				verbatimTextOutput('analysis'), 
	      				style = "overflow: auto;",
	      				class="well-panel"
	    			)
                )),
		tabsetPanel(
			id="resultTabset",
            tabPanel(
                fluidRow(br()),
                title="Run output and diagnostics"
            ),
            tabPanel(
                fluidRow(br()),
                title="Spectral plots",
	    		fluidRow(column(12,
            		h2("Spectral plot"),
	    			hr(),
        			htmlOutput("spectralTab")
        		))

            ),
            tabPanel(
                fluidRow(br()),
                title="Normalization plots"
            )
		)

	))
}
