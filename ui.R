# ui.R

require(DT)
require(shinyjs)
require(shinythemes)
require(shinyWidgets)

source("config/init_server_globals.R")
source("ui/analysisTab/analysisItem.R")
source("ui/runViewerTab/runViewerItem.R")
source("ui/databaseTab/databaseItem.R")
source("ui/aboutTab/aboutItem.R")

shinyUI(fluidPage(
    theme=shinytheme("united"),
    shinyjs::useShinyjs(),
    tags$head(
        tags$script(
            'Shiny.addCustomMessageHandler("changeProgressHeader",',
            'function(msg) {',
            '    $("#progressBarHeader_" + msg.tid).html(msg.value);',
            '});',
            'Shiny.addCustomMessageHandler("changeProgressFooter",',
            'function(msg) {',
            '    $("#progressBarFooter_" + msg.tid).html(msg.value);',
            '});'
        )
    ),
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
        title="cinnamoned",
        tabPanel("New analysis",icon=icon("flask"),
            analysisTabPanel()
        ),
        tabPanel("Analysis viewer",icon=icon("eye"),
            runViewerTabPanel()
        ),
        navbarMenu("Database",icon=icon("database"),
            tabPanel("Manage",icon=icon("eye"),
                databaseManageTabPanel()
            ),
            tabPanel("Modify",icon=icon("pencil"),
                databaseModifyTabPanel()
            ),
            tabPanel("Search",icon=icon("search"),
                databaseSearchTabPanel()
            )
        ),
        tabPanel("About",icon=icon("info"),
            aboutTabPanel()
        )
    ),
    # Footer
    absolutePanel(bottom = "0%", width = "98%", fixed = TRUE,
      fluidRow(column(12,
        div(
            class="footer",
            "Copyright ",HTML("&copy;"),"2018 ",
            a("RF Lab",href="http://renalfibrosis.fr/",target="_blank"),"/",
            a("Inserm",href="https://www.inserm.fr/",target="_blank"),", ",
            "Designed and maintened by ",
            a("HybridStat",href="http://www.hybridstat.com/",target="_blank")
        )
      ))
    )
))
