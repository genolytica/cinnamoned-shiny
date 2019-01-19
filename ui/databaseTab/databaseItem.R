databaseManageTabPanel <- function() {
  fluidRow(column(8,h1("View Past Runs"),
                  DT::dataTableOutput("runInfo")),
  column(4, 
    wellPanel(
      h2("Run Details"),
      hr(),
      htmlOutput('paramsInfo'), 
      style = "overflow: auto;",
      class="well-panel")
    )
  )
}

databaseModifyTabPanel <- function() {
  fluidRow(column(12,h1("Modify Metabolite")))
}

databaseSearchTabPanel <- function() {
  fluidRow(column(12,
    h1("Search Metabolite",
      fluidRow(column(4,
        wellPanel(
          h4("Select a Metabolite Filter:",
            radioButtons(
              inputId = "metaboliteFilters",
              label = "",
              choices = list(
                "m/z range"="mzRange",
                "Database ID"="hmdbID"
              )
            ),
            conditionalPanel(
              condition = "input.metaboliteFilters=='hmdbID'",
              fluidRow(column(10,
                textInput(
                  inputId = "metaboFiltersHmdbID",
                  label = "",
                  value = "",
                  placeholder = "Please type an HMDB ID"
                )
              ))
            ),
            conditionalPanel(
              condition = "input.metaboliteFilters=='mzRange'",
              fluidRow(column(5,
                textInput(
                  inputId = "lowerLimit",
                  label = "From:",
                  value = ""
                )
              ),
              column(5,
                     textInput(
                       inputId = "upperLimit",
                       label = "To:",
                       value = ""
                     )
              )
              )
            ),
          	column(3,
                 div(
                   class="pull-left",
                   style="display:inline-block",
                   actionButton(
                     inputId="calculateMetaboFilter",
                     label="Engage!",
                     icon=icon("rocket"),
                     class="btn-primary"
                   )
                 )
          ),fluidRow()
          ),class="well-panel"
        )
      )),
      fluidRow(  column(4, 
    wellPanel(
      h2("Filter to Query with"),
      hr(),
      verbatimTextOutput('filter'), 
      style = "overflow: auto;",
      class="well-panel")
    ))
    )
  ))
}
