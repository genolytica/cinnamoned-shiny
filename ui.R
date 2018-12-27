# ui.R

require(DT)
require(shinyjs)
require(shinythemes)

source("config/init_server_globals.R")
source("ui/analysisTab/analysisItem.R")
source("ui/databaseTab/databaseItem.R")

shinyUI(fluidPage(
    theme=shinytheme("united"),
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(
            rel="stylesheet",
            type="text/css",
            href="cinnamoned.css"
        )
    ),
    fluidRow(column(7,
        div(
            style="padding:0px; border-width:0px;",
            tags$img(src="logo.png",width="100%")
        )
    ),column(5,
        div(
            class="page-title",
            div(
                style="font-size:1.7em; font-weight:bold;",
                "CINNAMONED"
            ),
            "geometriCally INvariant NormAlization of MetabOlomic kidNEy Data"
        )
    )),
    navbarPage(
        id="cinamnavbar",
        title="cinnamoned2",
        tabPanel("Analysis",icon=icon("flask"),
            analysisTabPanel()
        ),
        navbarMenu("Database",icon=icon("database"),
            tabPanel("Manage",icon=icon("eye"),
                databaseTabPanel()
            ),
            tabPanel("Modify",icon=icon("pencil")
            ),
            tabPanel("Search",icon=icon("search")
            )
        ),
        tabPanel("About",icon=icon("user")
        )
    )
))
