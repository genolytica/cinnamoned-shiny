databaseManageTabPanel <- function() {
    fluidRow(column(7,
        wellPanel(
            h2("View past run info"),
            hr(),
            DTOutput("runInfo"),
            class="well-panel"
        )
    ),column(5, 
            wellPanel(
                h2("Run Details"),
                hr(),
                htmlOutput('paramsInfo'), 
                style = "overflow: auto;",
                class="well-panel"
            )
    ))
}
