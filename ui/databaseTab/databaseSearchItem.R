databaseSearchTabPanel <- function() {
    fluidRow(column(12,
        fluidRow(column(4,
            wellPanel(
                h2("Search Metabolite"),
                hr(),
                h4("Search metabolites by"),
                radioButtons(
                    inputId="metaboliteFilters",
                    label="",
                    inline=TRUE,
                    choices=list(
                        "m/z range"="mzRange",
                        "Database ID"="dbId"
                    )
                ),
                conditionalPanel(
                    condition="input.metaboliteFilters=='dbId'",
                    fluidRow(column(12,
                        selectizeInput(
                            inputId="metaboDbId",
                            label="",
                            choices=NULL,
                            multiple=TRUE
                        )
                    ))
                ),
                conditionalPanel(
                    condition = "input.metaboliteFilters=='mzRange'",
                    fluidRow(column(6,
                        textInput(
                            inputId="lowerLimit",
                            label="From",
                            value=""
                        )
                    ),column(6,
                        textInput(
                            inputId="upperLimit",
                            label="To",
                            value=""
                        )
                    ))
                ),
                #fluidRow(column(12,
                #    actionButton(
                #        inputId="fetchMetabolites",
                #        label="Comply!",
                #        icon=icon("hand-spock-o"),
                #        class="btn-primary pull-right"
                #    )
                #)),
                class="well-panel"
            )
        ),column(8,
            wellPanel(
                h2("Metabolite summary table"),
                hr(),
                DTOutput("metaboSummary"),
                class="well-panel"
            )
        )),
        fluidRow(column(12,
            wellPanel(
                h2("Metabolite details"),
                hr(),
                htmlOutput("metaboDetails"),
                class="well-panel"
            )
        ))
    ))
}

