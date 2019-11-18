runViewerTabPanel <- function() {
    fluidRow(column(12,
        fluidRow(column(8,
            wellPanel(
                h2("Select an analysis to view results"),
                hr(),
                DTOutput("pastRunInfo"),
                fluidRow(br()),
                fluidRow(column(12,
                    disabled(actionButton(
                        inputId="loadSelectedRun",
                        label="Load selected and comply!",
                        icon=icon("hand-spock-o"),
                        class="btn-primary pull-right"
                    ))
                )),
                class="well-panel"
            )
        ),column(4, 
            fluidRow(column(12,
                wellPanel(
                    h4("Normalization progress"),
                    hr(),
                    div(
                        id="normalizationProgressA",
                        style="font-size: 1.2em; margin-bottom: 5px;",
                        "Normalization progress (if required) will be ",
                        "displayed here"
                    ),
                    hidden(div(
                        id="progressWrapperA",
                        div(
                            class="progressbar-header",
                            id="progressBarHeader_normA",""
                        ),
                        progressBar(
                            id="normalizationProgressBarA",
                            value=0,
                            total=3,
                            display_pct=TRUE,
                            status="danger",
                            striped=TRUE
                        ),
                        div(
                            class="progressbar-footer",
                            id="progressBarFooter_normA",""
                        )
                    )),
                    class="well-panel"
                )
            ))
        )),
        fluidRow(column(12,
            h2("Results"),
            hr(),
            htmlOutput("pastRunResults")
        ))
    ))
}
