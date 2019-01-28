databaseSearchTabPanel <- function() {
    fluidRow(column(12,
        fluidRow(column(4,
            wellPanel(
                h2("Search Metabolite"),
                hr(),
                h4("Filter metabolite by"),
                radioButtons(
                    inputId="metaboliteFilters",
                    label="",
                    inline=TRUE,
                    choices=list(
                        "m/z range"="mzRange",
                        "Database ID"="hmdbID"
                    )
                ),
                conditionalPanel(
                    condition="input.metaboliteFilters=='hmdbID'",
                    fluidRow(column(12,
                        textInput(
                            inputId="metaboFiltersHmdbID",
                            label="",
                            value="",
                            placeholder="Please type an HMDB ID"
                        )
                    ))
                ),
                conditionalPanel(
                    condition = "input.metaboliteFilters=='mzRange'",
                    fluidRow(column(6,
                        textInput(
                            inputId = "lowerLimit",
                            label = "From:",
                            value = ""
                        )
                    ),column(6,
                        textInput(
                            inputId = "upperLimit",
                            label = "To:",
                            value = ""
                        )
                    ))
                ),
                fluidRow(column(12,
                    actionButton(
                        inputId="calculateMetaboFilter",
                        label="Go!",
                        icon=icon("rocket"),
                        class="btn-primary pull-right"
                    )
                )),
                class="well-panel"
            )
        ))
    ))
}
#~           ),class="well-panel"
#~         )
#~       )),
#~       fluidRow(  column(4, 
#~     wellPanel(
#~       h2("Filter to Query with"),
#~       hr(),
#~       verbatimTextOutput('filter'), 
#~       style = "overflow: auto;",
#~       class="well-panel")
#~     ))
#~     )
#~   ))
#~ }
