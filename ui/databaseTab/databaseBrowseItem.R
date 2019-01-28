databaseBrowseTabPanel <- function() {
	fluidRow(column(12,
		wellPanel(
			h2("Browse reference dataset"),
			hr(),
			div(
				style="overflow-x:auto;",
				DT::dataTableOutput("browseReferenceMetabolites")
			),
			class="well-panel"
		)
	))
}
