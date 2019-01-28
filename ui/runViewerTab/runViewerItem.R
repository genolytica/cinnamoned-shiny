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
                        label="Load selected",
                        icon=icon("truck"),
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


#~ runViewerTabPanel <- function() {
#~  fluidRow(column(8,h1("Run Output"),
#~      fluidRow(column(6,
#~          wellPanel(
#~                  h4("Type an analysis ID"),
#~                  hr(),
#~                  style = "overflow: auto;",
#~                  class="well-panel",
#~                  fluidRow(column(10,
#~                 textInput(
#~                  inputId = "analysisID",
#~                  label = "",
#~                  value = "",
#~                  placeholder = "e.g: 31082015120830"
#~                 )
#~                  )),
#~              column(6,
#~                     div(
#~                         class="pull-right",
#~                         style="display:inline-block",
#~                         disabled(actionButton(
#~                             inputId="runArchivedAnalysisViewer",
#~                             label="Engage!",
#~                             icon=icon("rocket"),
#~                             class="btn-primary"
#~                        ))
#~                     )
#~                 )
#~          )
#~      ),
#~                  column(6,
#~                     wellPanel(
#~                      h2("Analysis Directory (TEST)"),
#~                      hr(),
#~                      verbatimTextOutput('analysis'), 
#~                      style = "overflow: auto;",
#~                      class="well-panel"
#~                  )
#~                 )),
#~      tabsetPanel(
#~          id="resultTabset",
#~             tabPanel(
#~                 fluidRow(br()),
#~                 title="Run output and diagnostics"
#~             ),
#~             tabPanel(
#~                 fluidRow(br()),
#~                 title="Spectral plots",
#~              fluidRow(column(12,
#~                  h2("Spectral plot"),
#~                  hr(),
#~                  htmlOutput("spectralTab")
#~              ))

#~             ),
#~             tabPanel(
#~                 fluidRow(br()),
#~                 title="Normalization plots"
#~             )
#~      )

#~  ))
#~ }

