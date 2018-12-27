# R/Bioconductor packages required for Geniasis backed operation. They will
# auto-install dependencies.

# Sometimes there is a problem with https in virgin VMs...
source("http://bioconductor.org/biocLite.R")

if (!require(BiocInstaller)) {
    install.packages("BiocInstaller",
        repos="http://bioconductor.org/packages/3.7/bioc")
    require(BiocInstaller)
}

pkgs <- c("RNetCDF","xcms","CAMERA","Rdisop","DT","ggplot2","RCurl","tools",
    "RSQLite","shiny","shinyjs","shinythemes","colourpicker")

for (p in pkgs) {
    if (!require(p))
        biocLite(p) 
}
