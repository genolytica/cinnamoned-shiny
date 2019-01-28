databaseBrowseTabPanel <- function() {
    fluidRow(column(12,
        wellPanel(
            h2("Browse reference dataset"),
            hr(),
            div(
                style="overflow-x:auto;",
                DTOutput("browseReferenceMetabolites")
            ),
            class="well-panel"
        )
    ))
}
