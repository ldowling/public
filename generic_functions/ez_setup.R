ez_setup <- function(extras=NULL, install=FALSE){
  #list already installed packages
  inpacs <- rownames(installed.packages())
  #list of standard packages (modify as desired)
  spacs <- c("tidyverse","lubridate","data.table")
  #if user wants to install, do so for not yet installed standard packages
  if (install) {
    to_install <- spacs[!spacs%in%inpacs]
    if (length(to_install)>0) {
      install.packages(to_install, repos = "http://cran.us.r-project.org")
    }else{
      message("Standard packages already installed.")
    }
  }
  #load standard packages
  suppressMessages(library(tidyverse))
  suppressMessages(library(lubridate))
  suppressMessages(library(data.table))
  #if user wants to install, and has specified extra packages,
  # do so for not yet installed extra packages
  if (install&!is.null(extras)) {
    to_install <- extras[!extras%in%inpacs]
    if (length(to_install)>0) {
      install.packages(to_install, repos = "http://cran.us.r-project.org")
    }else{
      message("Extra packages already installed.")
    }
  }
  #load extra packages
  if (!is.null(extras)) {
    for (func in extras) {
      if (!func%in%.packages()) {
        suppressMessages(library(func,character.only=TRUE))
      }
    }
  }
  #report which packages beyond basic packages are now loaded
  bpacs <- c("stats","graphics","grDevices","utils","datasets","methods","base")
  message("Packages loaded:\n", sort(paste(.packages()[!.packages() %in% bpacs],
                                      collapse="; ")))
}


