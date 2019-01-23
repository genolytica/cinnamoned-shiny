# All the reactive variables exchanged throughout the application are initiated
# here. More will be added during porting...
initReactiveVars <- function() {
    reactiveVars <- new.env(parent=emptyenv())
    
    reactiveVars$pipelineControl <- reactiveValues(
        #step="preprocess",
        #step="timefilter",
        #step="normalization",
        step="result",
        filesUploaded=FALSE,
        sampleInfoFilled=FALSE,
        uiError=FALSE
    )
    
#~     reactiveVars$pipelineInput <- reactiveValues(
#~         currentRunId=NULL,
#~         basePath="/media/HD3/cprocess_tmp",
#~         runPath=NULL,
#~         dataPath=NULL,
#~         dataPathRaw=NULL,
#~         dataPathTrunc=NULL,
#~         diagPath=NULL,
#~         diagPathPreprocess=NULL,
#~         diagPathNormalization=NULL,
#~         scriptPath=NULL,
#~         sampleInfoFile=NULL,
#~         xcmsParamFile=NULL,
#~         xcmsLogFile=NULL,
#~         tmpXcmsLogFile=NULL,
#~         normLogFile=NULL,
#~         peaksRda=NULL,
#~         normRda=NULL,
#~         tmpPath="/media/HD3/ctmp",
#~         uploadedFiles=NULL,
#~         filenames=NULL,
#~         classes=NULL,
#~         refinedTimeBoundaries=NULL
#~     )
    
    reactiveVars$pipelineInput <- reactiveValues(
        currentRunId="21012019193711",
        basePath="/media/HD3/cprocess_tmp",
        runPath="/media/HD3/cprocess_tmp/21012019193711",
        dataPath="/media/HD3/cprocess_tmp/21012019193711/data",
        dataPathRaw="/media/HD3/cprocess_tmp/21012019193711/data/raw",
        dataPathTrunc="/media/HD3/cprocess_tmp/21012019193711/data/trunc",
        diagPath="/media/HD3/cprocess_tmp/21012019193711/diagnostic",
        diagPathPreprocess="/media/HD3/cprocess_tmp/21012019193711/diagnostic/preprocess",
        diagPathNormalization="/media/HD3/cprocess_tmp/21012019193711/diagnostic/normalization",
        scriptPath="/media/HD3/cprocess_tmp/21012019193711/scripts",
        sampleInfoFile="/media/HD3/cprocess_tmp/21012019193711/sample_info.txt",
        xcmsParamFile="/media/HD3/cprocess_tmp/21012019193711/scripts/xcms.yml",
        xcmsLogFile="/media/HD3/cprocess_tmp/21012019193711/scripts/xcms.Rout",
        tmpXcmsLogFile=NULL,
        normLogFile="/media/HD3/cprocess_tmp/21012019193711/scripts/norm.Rout",
        peaksRda="/media/HD3/cprocess_tmp/21012019193711/peaks.RData",
        normRda=NULL,
        tmpPath="/media/HD3/ctmp",
        uploadedFiles=NULL,
        filenames=c("2011-03-24_C3-3-3_NaCl_Run000010.cdf","2011-03-24_C3-3-3_NaCl_Run000011.cdf"),
        classes=c("Ctrl","Ctrl"),
        refinedTimeBoundaries=list(c(960,2700),c(960,2700))
    )
    
    load("/media/HD3/cprocess_tmp/21012019193711/peaks.RData")
    load("/media/HD3/cprocess_tmp/21012019193711/norm.RData")
    
    reactiveVars$pipelineResults <- reactiveValues(
        peaks=peaks,
        norm=norm,
        currentIndex=1
        #peaks=NULL,
        #norm=NULL
    )
    
    reactiveVars$timeFilter <- reactiveValues(
        do=TRUE,
        min=600,
        max=3000
    )
    
    reactiveVars$readSpec <- reactiveValues(
        nSlaves=1,
        profstep=1,
        profmethod="binlin"
    )
    
    reactiveVars$findPeaks <- reactiveValues(
        method=c("matchedFilter","centWave"),
        fwhm=30,
        sigma=7,
        max=5,
        step=0.1,
        steps=3,
        mzdiff=0.8-0.1*3,
        ppm=25,
        peakwidth=c(10,40),
        prefilter=c(3,100),
        mzCenterFun="wMean",
        integrate=1,
        mzdiff.cw=-0.001,
        fitgauss=FALSE,
        scanrange=integer(0),
        noise=0,
        verbose.columns=FALSE,
        snthresh=7,
        sleep=0
    )
    
    reactiveVars$groupPeaks <- reactiveValues(
        method=c("density","mzClust","nearest"),
        bw=c(50,30,20,10,5),
        mzwid=0.25,
        max=50,
        mzppm=20,
        mzabs=0,
        mzVsRTbalance=10,
        mzCheck=0.2,
        rtCheck=15,
        kNN=10,
        minfrac=0.5,
        minsamp=1,
        sleep=0
    )

    reactiveVars$retcor <- reactiveValues(
        method=c("loess","obiwarp"),
        missing=1,
        extra=1,
        smooth="loess",
        span=0.2,
        family="symmetric",
        profStep=1,
        center=NULL,
        response=1,
        score="cor",
        gapInit=0,
        gapExtend=0,
        factorDiag=2,
        factorGap=1,
        localAlignment=0,
        initPenalty=0,
        plottype="mdevden",
        col=NULL,
        ty=NULL
    )
    
    reactiveVars$extractPeaks <- reactiveValues(
        method="maxint",
        value="into",
        intensity="into"
    )
    
    reactiveVars$annotatePeaks <- reactiveValues(
        group="both",
        iso.flow="fwhm",
        sigma=6,
        perfwhm=0.6,
        pval=0.05,
        cor_eic_th=0.75,
        find.adducts=TRUE,
        maxiso=5,
        mzabs.add=0.01,
        mzabs.fiso=0.01,
        mzabs.diso=0.001,
        ppm.fiso=10,
        ppm.diso=5,
        polarity="positive",
        rulefile=NULL,
        filter.valid="valid",
        filter.score=0.75,
        peak.val="maxo",
        filter.dbe=NULL,
        write.output=NULL,
        run.par=TRUE,
        more.adducts=FALSE,
        fail.rules="internal",
        export.what="all"
    )
    
    reactiveVars$spectrePlots <- reactiveValues(
        spectreProfile=ggmessage("Spectre profiles will\nbe displayed here"),
        rendered=TRUE
    )
    
    reactiveVars$simpleTables <- reactiveValues(
        sampleTable=NULL
    )
    
    reactiveVars$reset <- function(which=c("all","preprocess","timefilter",
        "normalization")) {
        
        # Not much checking as the function is internal 
        switch(which,
            all = {
                reactiveVars$resetAll()
            },
            preprocess = {
                reactiveVars$resetPreprocess()
            },
            timefilter = {
                reactiveVars$resetTimefilter()
            },
            normalization = {
                reactiveVars$resetNormalization()
            }
        )
    }
    
    reactiveVars$resetAll <- function() {
        # Those with a supporting function
        reactiveVars$resetPreprocess()
        reactiveVars$resetTimefilter()
        reactiveVars$resetNormalization()
        reactiveVars$resetResults()
        
        # And the rest
         reactiveVars$spectrePlots <- reactiveValues(
            spectreProfile=ggmessage(
                "Spectre profiles will\nbe displayed here"),
            rendered=TRUE
        )
        # TODO: Fill the rest please
    }
    
    reactiveVars$resetPreprocess <- function() {
        # Reset readSpec, findPeaks, uploadedFiles
        # TODO: Fill the function - DONE?
      
        # Reset readSpec
        reactiveVars$readSpec$profstep  <- 1
        # Reset findPeaks
        reactiveVars$findPeaks$snthresh <- 7
        reactiveVars$findPeaks$step     <- 0.1
        reactiveVars$findPeaks$fwhm     <- 30
        reactiveVars$findPeaks$sigma    <- 7
        reactiveVars$findPeaks$steps    <- 3
        reactiveVars$findPeaks$max      <- 5
        # Reset uploadFiles
        
    }

    reactiveVars$resetTimefilter <- function() {
        # Reset timeFilter
        reactiveVars$timeFilter$do  <- TRUE
        reactiveVars$timeFilter$min <- 600
        reactiveVars$timeFilter$max <- 3000
    }

    reactiveVars$resetNormalization <- function() {
        reactiveVars$normPeaks$method           <- 'geom'
        reactiveVars$normPeaks$correctfor       <- 'both'
        reactiveVars$normPeaks$mztol            <- 0.1
        reactiveVars$normPeaks$diagPlotsInclude <- TRUE
        reactiveVars$normPeaks$export           <- 'all'
        reactiveVars$normPeaks$tspan            <- 0
        reactiveVars$normPeaks$it               <- 3
        reactiveVars$normPeaks$corrfac          <- 2
        reactiveVars$normPeaks$cutq             <- 0.98
        reactiveVars$normPeaks$diagPlots        <- 'rlm'
        reactiveVars$normPeaks$ispan            <- 0
        reactiveVars$normPeaks$corrfacNS        <- 2
        reactiveVars$normPeaks$method           <- 'geom'
    }
    
    reactiveVars$resetResults <- function() {
        reactiveVars$pipelineResults$peaks <- NULL
    }
    
    return(reactiveVars)
}

initReactiveMsgs <- function() {
    analysisMessages <- reactiveValues(
        messages=list(
            list(
                type="INFO",
                msg=paste(getTime("INFO"),"Welcome to the analysis panel of ",
                    "cinnamoned! This is an info message. Make your ",
                    "selections on the left.")
            )
        )
    )
    
    return(list(
        analysisMessages=analysisMessages
    ))
}

ggmessage <- function(msg="",type=c("generic","info","success","warning",
    "error"),size=c("large","small")) {
    type <- tolower(type[1])
    size <- tolower(size[1])
    switch(type,
        generic = { color <- "black" },
        info = { color <- "green2" },
        success = { color <- "blue2" },
        warning = { color <- "orange" },
        error = { color <- "red2" }
    )
    switch(size,
        large = { s <- 10 },
        small = { s <- 5 }
    )
    return(
        ggplot(data=data.frame(x=1:100,y=1:100)) + 
            geom_text(data=data.frame(x=50,y=50,label=msg),
                aes(x=x,y=y,label=label),colour=color,size=s) +
            theme(
                axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank()
            )
    )
}

disableActionButton <- function(id,session) {
    session$sendCustomMessage(
        type="jsCode",
        list(code=paste("$('#",id,"').prop('disabled',true)",sep=""))
    )
}
