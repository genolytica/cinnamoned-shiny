# cinnamoned
geometriCally INvariant NormAlization of MetabOlomic kidNEy Data remastered!

## Porting of the cINNaMoneD application to shiny!

cINNaMoneD is a web-based application, which performs peak detection, noise filtering, retention time alignment and intensity normalization in one ore more samples of CE/MS data, coming from urine derived from human patients with a variety of renal diseases. Each of the above steps is performed per sample. The procedure starts by peak detection using the matchedFilter algorithm of the Bioconductor package [xcms](https://bioconductor.org/packages/release/bioc/html/xcms.html) [(Smith et al., 2006)](http://www.ncbi.nlm.nih.gov/pubmed/16448051). Then for each sample, the detected masses are matched to a reference dataset, consisting of 6044 mz/retention time pairs, derived from 75 CE/MS urine samples including control and healthy samples as well as samples extracted from a variety of renal diseases. The matched mz pairs between new samples and the reference dataset, contain a subset of invariant mz values across most reference samples. These invariant mz/retention time pairs are used for the time alignment of the new samples to the reference dataset, as wel as for intensity scaling so that the new samples become comparable with the reference dataset and among themselves. The time alignment is based on a clustering procedure along the matched invariant entities coupled with local regression and the intensity scaling is performed using a local regression curve constructed using the intensity of the invariant entities and e reference sample which is the weighted mean of the matched mz/retention time pairs from the reference dataset.

The reference dataset was initially processed, fine tuned, annotated and summarized using the Bioconductor package xcms [(Smith et al., 2006)](http://www.ncbi.nlm.nih.gov/pubmed/16448051), the Bioconductor package [CAMERA](https://bioconductor.org/packages/release/bioc/html/CAMERA.html) [(Kuhl, et al., 2012)](http://www.ncbi.nlm.nih.gov/pubmed/22111785), several in-house routines. The dataset was furthermore annotated using a local installation of the [ChEBI](http://www.ebi.ac.uk/chebi/) small molecule database, the Human Metabolome Database ([HMDB](http://www.hmdb.ca/)) and the publicly available web services of Kyoto Encyclopedia for Genes and Genomes ([KEGG](http://www.genome.jp/kegg/)). The invariant mz/retention time pairs were selected using an automated method based on geometrical criteria coupled with an iterative selection algorithm that minimizes a variability measure of the invariant set and assesses the efficiency based on the effect of normalization of the total dataset.

In January 2019 cINNaMoneD was wholy ported to R using the [Shiny](https://shiny.rstudio.com/) web development framework in order to achieve maximum stability and to ease maintenance issues. In addition, the backend algorithms were brought up-to-date to further improve accuracy.

## Installation instructions

### Required R/Bioconductor packages

The rest assumes that a recent (if not the latest) R version exist on your system and basic knowledge on how to install packages as an administrator (```sudo``` or related on Linux systems, right-click "Run R as administrator" in Windows). Assuming now that you have started an R session as admin, you should download and install some [CRAN](https://cran.r-project.org/) and [Bioconductor](https://bioconductor.org/) packages:

```
source("http://bioconductor.org/biocLite.R")

if (!require(BiocInstaller)) {
    install.packages("BiocInstaller",
        repos="http://bioconductor.org/packages/3.7/bioc")
    require(BiocInstaller)
}

pkgs <- c("RNetCDF","xcms","CAMERA","DT","ggplot2","RCurl","tools",
    "RSQLite","shiny","shinyjs","shinythemes","colourpicker","yaml",
    "shinyWidgets","utils")

for (p in pkgs)
   biocLite(p)
```

### cinnamoned application

The application should live either where the Shiny apps live (usually ```/srv/shiny-server``` in Linux) or if you want to play with it, in a development directory. We assume production in the following:

```
$ cd /srv/shiny-server
$ git clone https://github.com/hybridstat/cinnamoned-shiny.git
```

Than, under ```config``` directory you must create the file ```base_path.txt``` which simply indicates where the data that the application generate are stored. If cinnamoned does not find this file or the directory there does not exist, there is a legacy hardcoded fallback.

```
$ cd cinnamoned-shiny/config
$ cat "/my/run/path" >> base_path.txt
$ cd ../../
```

The above only install the application but not the required SQLite databases to run, so it may launch but not run. The main data database is the property of the Renal Fibrosis Lab, U1048, Toulouse France and will not be posted online. Please contact the lab directly if interested. The runtime database schema will be posted here soon.
