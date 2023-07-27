# List of packages for session
.packages = c("tidyverse","stringr","R.utils",
              "magrittr","future","parallel","doSNOW","XMLSchema", "SSOAP","here")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

index <- 1:8

open_files <- function(this.index){

  rstudioapi::navigateToFile(paste0("/home/atlantis/psatlantismodel/Atlantis_mv_",this.index,"/AMPSbioparam_mv",this.index,"_2022.prm"))
  
    
}

lapply(index, open_files)


library("diffr")

diffr("AMPSbioparamV6_B1984.prm", "AMPSbioparam_mv1_2022.prm", contextSize = 3, minJumpSize = 10, wordWrap = TRUE,
      before = "AMPSbioparamV6_B1984.prm", after = "AMPSbioparam_mv1_2022.prm", width = NULL, height = NULL)


grep -rnw '/home/atlantis/trunk/' -e 'corrupted size'
grep -Ril "corrupted size" /