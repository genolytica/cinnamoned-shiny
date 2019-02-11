# Init globals (don't want to laod these everytime a user connects and they 
# don't take much time to load)

# Load basic packages
require(shiny)
#require(shinyjs)
#require(shinyWidgets)
#require(colourpicker)

# Load backend functions
source("lib/annotatePeaks.R")
source("lib/diagplots.R")
source("lib/filterBrukerNetCDF.R")
source("lib/general.R")
source("lib/normalizePeaks.R")
source("lib/normalizeSamples.R")
source("lib/queryDBs.R")
source("lib/xcmsPipeline.R")

baseColours <- c("#B40000","#00B400","#0000B4","#B45200","#9B59B6","#21BCBF",
    "#BC4800","#135C34","#838F00","#4900B5")

# Restrict the number of cores dedicated to cinnamoned2
RC <- 0.25

# Database paths
METABO_DB <- "data/RFLab_MetaboDB.sqlite"
APP_DB <- "data/cinnamonDB.sqlite"

if (file.exists("config/base_path.txt")) {
	BASE_PATH <- as.character(read.delim("config/base_path.txt",
		header=FALSE)[1,1])
	if (!dir.exists(BASE_PATH)) { # Fallback to hardcoded legacy
		BASE_PATH <- "/media/HD3/cprocess"
		dir.create(BASE_PATH,recursive=TRUE)
	}
}

# Error messages
ERROR_MESSAGES <- list(
    projectName=paste("The project name must be smaller than 100", 
        "characters and cannot contain the special characters",
        "\ / @ # $ & * ` ! ( ) % ^ , . < > ? | ' ; [ ] \" or space"),
    filterTimeMin="Minimum time must be an integer greater than 0!",
    filterTimeMax="Maximum time must be an integer greater than 0!",
    filterTimeComparison="Maximum time must be greater than Minimum time!",
    profileStep="Profile reading step must be an integer greater than 0!",
    xcmsSNR="Signal to noise ratio must be a real number greater than 0!",
    xcmsEIBPCSize="EIBPC step size must be a real number greater than 0!",
    xcmsFWHM="Full width at half maximum must be an integer greater than 0!",
    xcmsSigma="Peak model standard deviation (sigma) must be an integer >= 0!",
    xcmsEIBPCSteps="EIBPC steps to combine must be an integer greater than 0!",
    xcmsEIBPCMaxPeaks=paste("Maximum peaks per EIBPC must be an integer",   
        "greater than 0!"),
    mzTol="m/z tolerance must be a real number greater than 0!",
    tSpan="LOESS span must be a real number >= 0!",
    It="Alignment algorithm iterrations must be a real number >= 0!",
    corrFac="LOESS singularity correction factor must be a real number >= 0!",
    cutQ="RT deviation exclusion quantile real number >= 0!",
    iSpan="LOESS span must be a real number >= 0!",
    corrFacNS="Non-standards correction factor must be a real number >= 0!",
    analysisWriteError=paste("An error has been occured while writing results",
        "the database! Please report to the administrator with the analysis",
        "ID above.")
)

# Database queries
DB_QUERIES <- list(
    INFO_ALL=paste('SELECT `run_id`,`project_name`,`date` FROM `run_info`',
        'ORDER BY `date`'),
    INFO_DATES_1=paste('SELECT `run_id`,`project_name`,`date` FROM `run_info`',
        'WHERE `date`>='),
    INFO_DATES_2=' AND `date`<=',
    INFO_DATES_3=' ORDER BY `date` DESC',
    INFO_PARAMS=paste('SELECT `ref_run_id`, `xcms_filter_do`,',
        '`xcms_filter_min`, `xcms_filter_max`, `xcms_read_profstep`,',
        '`xcms_read_profmethod`, `xcms_find_snthresh`, `xcms_find_step`,',  
        '`xcms_find_fwhm`, `xcms_find_sigma`, `xcms_find_steps`,',
        '`xcms_find_max`, `xcms_find_mzdiff`, `norm_method`, `norm_tol`,',
        '`norm_correctfor`, `norm_export`, `norm_diagplot`, `norm_tspan`,',
        '`norm_tit`, `norm_corrfac`, `norm_cutq`, `norm_normalize`,',
        '`norm_ispan`, `norm_cutrat`, `norm_times` FROM `run_parameters`',
        'WHERE `ref_run_id`='),
    CLASS_DATA=paste('SELECT `project_name`, `project_path`, `class_file` FROM',
        '`run_info` WHERE `run_id`='),
    DIAG_DATA=paste('SELECT `project_name`, `project_path`,',
        '`diagnostic_normalization_path`, `class_file` FROM `run_info` WHERE',
        '`run_id`='),
    RES_DATA='SELECT `project_path` FROM `run_info` WHERE `run_id`=',
    AUTO_METAB_1='SELECT `id` FROM `peak_info` WHERE `id` LIKE',
    AUTO_METAB_2='ORDER BY `id`',
    METAB_BY_RANGE_1=paste('SELECT `id`, `mz`, `rt`, `is_geom`, `is_rlm`,',
        '`is_both` FROM `peak_info` WHERE `mz`>='),
    METAB_BY_RANGE_2='AND `mz`<=',
    METAB_BY_RANGE_3='ORDER BY `mz`',
    METAB_BY_ID_1=paste('SELECT `id`, `mz`, `rt`, `is_geom`, `is_rlm`,',
        '`is_both` FROM `peak_info` WHERE `id` IN'),
    METAB_BY_ID_2='ORDER BY `mz`',
    METAB_INFO=paste('SELECT `id`, `mz`, `rt`, `mzmin`, `mzmax`, `rtmin`,',
        '`rtmax`, `isotopes`, `adduct`, `real_mass`, `prop_formula`,',
        '`theor_mass`, `summarized_intensity_geom`,',
        '`summarized_intensity_rlm`, `summarized_intensity_both`,',
        '`is_geom`, `is_rlm`, `is_both` FROM `peak_info` WHERE `id`='),
    METAB_AN_1=paste('SELECT `hmdb_id`, `hmdb_formula`, `hmdb_name`,',
        '`kegg_formula`, `kegg_name`, `chebi_id`, `chebi_formula`,',
        '`chebi_name` FROM `meta_data` WHERE ABS(`real_mass`-'),
    METAB_AN_2=')<=0.00000001',
    DELETE_RUN='DELETE FROM `run_info` WHERE `run_id`=',
    RUN_INFO_ALL=paste('SELECT `run_id`, `project_name`, `date` FROM',
        '`run_info` ORDER BY `date` DESC'),
    METAB_ALL_INFO=paste('SELECT `id`, `mz`, `rt`, `mzmin`, `mzmax`, `rtmin`,',
        '`rtmax`, `isotopes`, `adduct`, `real_mass`, `prop_formula`,',
        '`theor_mass`, `summarized_intensity_geom`,',
        '`summarized_intensity_rlm`, `summarized_intensity_both`,',
        '`is_geom`, `is_rlm`, `is_both` FROM `peak_info`'),
    NORM_INFO_PARAMS=paste('SELECT `ref_run_id`, `norm_method`, `norm_tol`,',
        '`norm_correctfor`, `norm_export`, `norm_diagplot`, `norm_tspan`,',
        '`norm_tit`, `norm_corrfac`, `norm_cutq`, `norm_normalize`,',
        '`norm_ispan`, `norm_cutrat`, `norm_times` FROM `run_parameters`',
        'WHERE `ref_run_id`='),
    METAB_ALL_ID=paste('SELECT `id` FROM `peak_info` ORDER BY `id`')
)

# Maximum filesize
options(shiny.maxRequestSize=10*1024^3)
