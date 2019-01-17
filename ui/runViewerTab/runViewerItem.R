runViewerTabPanel <- function() {
	fluidRow(column(8,h1("Run Output"),
		tabsetPanel(
			id="resultTabset",
                tabPanel(
                    fluidRow(br()),
                    title="Diagnostic plots"
                ),
                tabPanel(
                    fluidRow(br()),
                    title="MA plots"
                ),
                tabPanel(
                    fluidRow(br()),
                    title="Results table"
                )
		)
		))
}
