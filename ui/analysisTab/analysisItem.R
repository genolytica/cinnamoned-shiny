analysisTabPanel <- function() {
    fluidRow(
        conditionalPanel(
            condition="output.panelStatus=='preprocess'",
            analysisTabPanelPreprocess()
        ),
        conditionalPanel(
            condition="output.panelStatus=='timefilter'",
            analysisTabPanelTimefilter()
        ),
        conditionalPanel(
            condition="output.panelStatus=='normalization'",
            analysisTabPanelNormalization()
        ),
        conditionalPanel(
            condition="output.panelStatus=='result'",
            analysisTabPanelResult()
        )
    )
}

analysisTabPanelPreprocess <- function() {
    fluidRow(column(5,
        fluidRow(column(12,
            wellPanel(
                h4("Project information"),
                hr(),
                fluidRow(column(12,
                    textInput(
                        inputId="projectName", 
                        label="Project name (100 chars max)",
                        value="",
                        placeholder="Project name"
                    ),
                    div(id="projectNameError",class="input-error",
                        ERROR_MESSAGES$projectName)
                )),
                fluidRow(column(12,
                    fileInput(
                        inputId="projectFiles", 
                        label="Upload NetCDF files",
                        multiple=TRUE,
                        accept=c(
                            "application/x-netcdf",
                            "application/x-netcdf4"
                        )
                    )
                )),
                fluidRow(column(6,
                    div(
                        style="font-weight:bold;",
                        "Retention time filtering"
                    )
                ),column(6,
                    div(
                        style="margin-top: -10px;",
                        checkboxInput(
                            inputId="performRtFiltering",
                            label="Perform",
                            value=TRUE
                        )
                    )
                )),
                conditionalPanel(
                    condition="input.performRtFiltering",
                    fluidRow(column(6,
                        numericInput(
                            inputId="filterTimeMin", 
                            label="Min (seconds)", 
                            value=600,
                            min=0
                        ),
                        div(id="filterTimeMinError",class="input-error",
                            ERROR_MESSAGES$filterTimeMin)
                    ),column(6,
                        numericInput(
                            inputId="filterTimeMax", 
                            label="Max (seconds)", 
                            value=3000,
                            min=0
                        ),
                        div(id="filterTimeMaxError",class="input-error",
                            ERROR_MESSAGES$filterTimeMax)
                    ),
                    div(id="filterTimeCompError",class="input-error",
                        ERROR_MESSAGES$filterTimeComparison)
                    )
                ),
                class="well-panel"
            )
        )),
        fluidRow(column(12,
            wellPanel(
                h4("Sample(s) information"),
                hr(),
                fluidRow(column(12,
                    fileInput(
                        inputId="sampleInfoFile", 
                        label="Upload sample-class relationship file",
                        accept=c("text/*")
                    )
                )),
                fluidRow(column(12,
                    div(
                        style="font-weight:bold; font-size:1.1em",
                        "OR"
                    )
                )),
                fluidRow(column(12,
                    p(
                        
                        paste("Manually enter sample-class information ",
                            "(after file upload)",sep="")
                    )
                )),
                fluidRow(column(6,
                    div(
                        style=paste("font-weight:bold;",
                            "border-style: none none dashed none;",
                            "border-width: 2px;"),
                        "Filename"
                    )
                ),column(6,
                    div(
                        style=paste("font-weight:bold;",
                            "border-style: none none dashed none;",
                            "border-width: 2px;"),
                        "Class"
                    )
                )),
                fluidRow(column(12,
                    div(class="small",
                        htmlOutput("sampleInfoEdit")
                    )
                )),
                class="well-panel"
            )
        ))
    ),column(7,
        fluidRow(column(12,
            wellPanel(
                h4("Peak detection parameters"),
                hr(),
                fluidRow(column(12,
                    radioButtons(
                        inputId="xcmsDefaultParameters",
                        label="Use default xcms parameters",
                        inline=TRUE,
                        choices=list(
                            "Use defaults"="defaults",
                            "Customize"="custom"
                        )
                    ),
                    div(id="profStepError",class="input-error",
                        ERROR_MESSAGES$profStep)
                )),
                conditionalPanel(
                    condition="input.xcmsDefaultParameters=='custom'",
                    tabsetPanel(
                        id="xcmsSettings",
                        tabPanel(
                            fluidRow(br()),
                            title="Spectre reading",
                            fluidRow(column(6,
                                numericInput(
                                    inputId="profileStep", 
                                    label="Profile reading step", 
                                    value=1,
                                    min=1
                                ),
                                div(id="profileStepError",class="input-error",
                                    ERROR_MESSAGES$profileStep)
                            ),column(6,
                                selectInput(
                                    inputId="profileMethod",
                                    label="Profile generation method",
                                    choices=list(
                                        "bin"="bin",
                                        "binlin"="binlin",
                                        "binlinbase"="binlinbase",
                                        "intlinbase"="intlinbase"
                                    ),
                                    selected="binlin"
                                )
                            ))
                        ),
                        tabPanel(
                            fluidRow(br()),
                            title="Peak detection",
                            fluidRow(column(4,
                                textInput(
                                    inputId="xcmsSNR", 
                                    label="Signal-to-noise ratio", 
                                    value="7"
                                ),
                                div(id="xcmsSNRError",class="input-error",
                                    ERROR_MESSAGES$xcmsSNR)
                            ),column(4,
                                textInput(
                                    inputId="xcmsEIBPCSize", 
                                    label="EIBPC step size", 
                                    value="0.1"
                                ),
                                div(id="xcmsEIBPCSizeError",class="input-error",
                                    ERROR_MESSAGES$xcmsEIBPCSize)
                            ),column(4,
                                textInput(
                                    inputId="xcmsFWHM", 
                                    label="Full width at half maximum", 
                                    value="30"
                                ),
                                div(id="xcmsFWHMError",class="input-error",
                                    ERROR_MESSAGES$xcmsFWHM)
                            )),
                            fluidRow(column(4,
                                textInput(
                                    inputId="xcmsSigma", 
                                    label="Peak model standard deviation", 
                                    value="7"
                                ),
                                div(id="xcmsSigmaError",class="input-error",
                                    ERROR_MESSAGES$xcmsSigma)
                            ),column(4,
                                textInput(
                                    inputId="xcmsEIBPCSteps", 
                                    label="EIBPC combine steps", 
                                    value="3"
                                ),
                                div(id="xcmsEIBPCStepsError",class="input-error",
                                    ERROR_MESSAGES$xcmsEIBPCSteps)
                            ),column(4,
                                textInput(
                                    inputId="xcmsEIBPCMaxPeaks", 
                                    label="Maximum peaks per EIBPC", 
                                    value="5"
                                ),
                                div(id="xcmsEIBPCMaxPeaksError",class="input-error",
                                    ERROR_MESSAGES$xcmsEIBPCMaxPeaks)
                            ))
                        )
                    )
                ),
                fluidRow(br()),
                fluidRow(column(6,
                    div(
                        class="pull-left",
                        style="display:inline-block",
                        actionButton(
                            inputId="resetPreprocessing",
                            label="Reset",
                            icon=icon("undo")
                        )
                    )
                ),column(2," "
                ),column(6,
                    div(
                        class="pull-right",
                        style="display:inline-block",
                        disabled(actionButton(
                            inputId="runPreprocessing",
                            label="Engage!",
                            icon=icon("rocket"),
                            class="btn-primary"
                       ))
                    )
                )),
                class="well-panel"
            )
        )),
        fluidRow(column(12,
            wellPanel(
                h4("Analysis progress"),
                hr(),
                div(
                    id="analysisProgress",
                    style="font-size: 1.2em; margin-bottom: 5px;",
                    "Analysis progress will be displayed here"
                ),
                hidden(div(
                    id="progressWrapper",
                    div(
                        class="progressbar-header",
                        id="progressBarHeader_pre",""
                    ),
                    progressBar(
                        id="preprocessProgressBar",
                        value=0,
                        total=15,
                        display_pct=TRUE,
                        status="danger",
                        striped=TRUE
                    ),
                    div(
                        class="progressbar-footer",
                        id="progressBarFooter_pre",""
                    )
                )),
                class="well-panel"
            )
        )),
        fluidRow(column(12,
            wellPanel(
                h4("Sample info"),
                hr(),
                tableOutput("sampleInfoTable"),
                class="well-panel"
            )
        ))
    ))
}

analysisTabPanelTimefilter <- function() {
    fluidRow(column(12,
        wellPanel(
            fluidRow(column(12,
                h2("Review retiention time boundaries"),
                h3(paste("Please inspect the following spectral figures of ",
                "the detected peaks and adjust time as necessary.")),
                hr(),
                htmlOutput("spectralInspection")
            )),
            fluidRow(column(4,
                div(
                    class="pull-left",
                    style="display:inline-block",
                    actionButton(
                        inputId="resetToBack",
                        label="Reset all",
                        icon=icon("exclamation-triangle")
                    )
                ),
                div(
                    class="pull-right",
                    style="display:inline-block",
                    actionButton(
                        inputId="resetTimeBoundaries",
                        label="Reset times",
                        icon=icon("undo")
                    )
                )
            ),column(8,
                div(
                    class="pull-right",
                    style="display:inline-block",
                    actionButton(
                        inputId="proceedToNormalization",
                        label="Next",
                        icon=icon("arrow-right"),
                        class="btn-primary"
                    )
                )
            )),
            class="well-panel"
        )
    ))
}

analysisTabPanelNormalization <- function() {
    fluidRow(column(6,
        fluidRow(column(12,
            wellPanel(
                h4("Internal standards normalization parameters"),
                hr(),
                radioButtons(
                    inputId="changeNormalizationParameters",
                    label="Use default normalization parameters",
                    inline=TRUE,
                    choices=list(
                      "Use defaults"="defaults",
                      "Customize"="custom"
                    )
                ),
                conditionalPanel(
                    condition="input.changeNormalizationParameters=='custom'",
                    tabsetPanel(
                        id="normalizationParameters",
                        tabPanel(
                            h5("General"),
                            fluidRow(br()),
                            fluidRow(column(4,
                                selectInput(
                                    inputId="method", 
                                    label="Standards selection method", 
                                    choices=list(
                                        "geometrical"="geom",
                                        "robust linear model"="rlm",
                                        "both"="both"
                                    ),
                                    selected="geom"
                                )
                            ),column(4,
                                selectInput(
                                    inputId="correctfor", 
                                    label="Correct for", 
                                    choices=list(
                                        "time-intensity"="both",
                                        "time"="time",
                                        "intensity"="intensity",
                                        "none"="none"
                                    ),
                                    selected="both"
                                )
                            ),column(4,
                                textInput(
                                    inputId="mztol", 
                                    label="m/z tolerance", 
                                    value="0.01"
                                ),
                                div(
                                    id="mzTolError",class="input-error",
                                    ERROR_MESSAGES$mzTol
                                )
                            )),
                            fluidRow(column(4,
                                selectInput(
                                    inputId="export", 
                                    label="Export results", 
                                    choices=list(
                                        "Do not export"="none",
                                        "All data"="all",
                                        "Gene ARMADA"="armada"
                                    ),
                                    selected="armada"
                                )
                            ),column(4," "
                            ),column(4,
                                div(
                                    style="font-weight:600; font-size:1em;",
                                    "Plot diagnostics"
                                ),
                                div(
                                    style="margin-top: 4px;",
                                    switchInput(
                                        inputId="diagPlotsInclude", 
                                        label="Switch", 
                                        onStatus="danger",
                                        value=TRUE
                                    )
                                )
                            ))
                        ),
                        tabPanel(
                            h5("Retention time alignment"),
                            fluidRow(br()),
                            class="well-panel",
                            fluidRow(column(6,
                                textInput(
                                    inputId="tspan", 
                                    label="LOESS span", 
                                    value="0"
                                ),
                                div(
                                    id="tSpanError",class="input-error",
                                    ERROR_MESSAGES$tSpan
                                )
                            ),column(6,
                                textInput(
                                    inputId="tit", 
                                    label="Alignment algorithm iterrations", 
                                    value="3"
                                ),
                                div(id="itError",class="input-error",
                                    ERROR_MESSAGES$It)
                            )),
                            fluidRow(column(6,textInput(
                                inputId="corrfac", 
                                    label=paste("LOESS singularity ",   
                                        "correction factor",sep=""), 
                                    value="2"
                                ),
                                div(id="corrFacError",class="input-error",
                                    ERROR_MESSAGES$corrFac)
                            ),column(6,
                                textInput(
                                    inputId="cutq", 
                                    label="RT deviation exclusion quantile",
                                    value="0.98"
                                ),
                                div(id="cutQError",class="input-error",
                                    ERROR_MESSAGES$cutQ)
                            ))
                        ),
                        tabPanel(
                            h5("Intensity normalization"),
                            fluidRow(br()),
                            class="well-panel",
                                fluidRow(column(6,
                                    selectInput(
                                        inputId="normalize", 
                                        label = "Normalization method", 
                                        choices = list(
                                            "LOESS"="loess",
                                            "Robust Linear Model (RLM)"="rlm",
                                            "Linear Model (LM)"="lm",
                                            "IS factor"="simple"
                                            ),
                                            selected="rlm"
                                    )
                                ),column(6,
                                    textInput(
                                        inputId="ispan", 
                                        label="LOESS span:", 
                                        value="0"
                                    ),
                                    div(id="iSpanError",class="input-error",
                                        ERROR_MESSAGES$iSpan)
                              )),
                              fluidRow(column(6,
                                  textInput(
                                      inputId="corrfacNS", 
                                      label="Non-standards correction factor", 
                                      value="2"
                                  ),
                                  div(id="corrFacNSError",class="input-error",
                                      ERROR_MESSAGES$corrFacNS)
                              ))
                        )
                    )
                ),
                fluidRow(br()),
                fluidRow(column(6,
                    div(
                        class="pull-left",
                        style="display:inline-block",
                        actionButton(
                            inputId="resetNormalization",
                            label="Reset",
                            icon=icon("undo")
                        )
                    )
                ),column(2," "
                ),column(6,
                    div(
                        class="pull-right",
                        style="display:inline-block",
                        actionButton(
                            inputId="runNormalization",
                            label="Energize!",
                            icon=icon("bolt"),
                            class="btn-primary"
                        )
                    )
                )),
                class="well-panel"
            )
        ))
    ),column(6,
        fluidRow(column(12,
            wellPanel(
                h4("Normalization progress"),
                hr(),
                div(
                    id="normalizationProgress",
                    style="font-size: 1.2em; margin-bottom: 5px;",
                    "Normalization progress will be displayed here"
                ),
                hidden(div(
                    id="progressWrapperN",
                    div(
                        class="progressbar-header",
                        id="progressBarHeader_norm",""
                    ),
                    progressBar(
                        id="normalizationProgressBar",
                        value=0,
                        total=3,
                        display_pct=TRUE,
                        status="danger",
                        striped=TRUE
                    ),
                    div(
                        class="progressbar-footer",
                        id="progressBarFooter_norm",""
                    )
                )),
                class="well-panel"
            )
        ))
    ))
}

analysisTabPanelResult <- function() {
    fluidRow(column(12,
        h2("Results"),
        hr(),
        htmlOutput("resultsPage")
    ))
}
