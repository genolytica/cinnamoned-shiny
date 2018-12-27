# All the reactive variables exchanged throughout the application are initiated
# here. More will be added during porting...
initReactiveVars <- function() {
    timeFilter <- reactiveValues(
        do=TRUE,
        min=600,
        max=3000
    )
    
    readSpec <- reactiveValues(
        nSlaves=1,
        profstep=1,
        profmethod="binlin"
    )
    
    findPeaks <- reactiveValues(
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
    
    groupPeaks <- reactiveValues(
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
    
    retcor <- reactiveValues(
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
    
    extractPeaks <- reactiveValues(
        method="maxint",
        value="into",
        intensity="into"
    )
    
    annotatePeaks <- reactiveValues(
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
    
    spectrePlots <- reactiveValues(
        spectreProfile=ggmessage("Spectre profiles will\nbe displayed here"),
        rendered=TRUE
    )
    
    currentTables <- reactiveValues()
    
    return(list(
        timeFilter=timeFilter,
        readSpec=readSpec,
        findPeaks=findPeaks,
        groupPeaks=groupPeaks,
        retcor=retcor,
        extractPeaks=extractPeaks,
        annotatePeaks=annotatePeaks,
        spectrePlots=spectrePlots
    ))
}

initReactiveMsgs <- function() {
    analysisMessages <- reactiveValues(
        messages=list(
            list(
                type="INFO",
                msg=paste(getTime("INFO"),"Welcome to the analysis panel of ",
                    "cinnamoned2! This is an info message. Make your ",
                    "selections on the left.")
            )
        )
    )
    
    return(list(
        analysisMessages=analysisMessages
    ))
}

resetReactiveVars <- function(allReactiveVars) {
    allReactiveVars$timeFilter$do <- TRUE
    allReactiveVars$timeFilter$min <- 600
    allReactiveVars$timeFilter$max <- 3000
    
    # Fill the rest please...
    
    return(allReactiveVars)
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
