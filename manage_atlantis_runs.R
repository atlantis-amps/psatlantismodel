#' Code to manage and run Atlantis simulations using parallel processing and doAzure
#'  
#' @author Hem Nalini Morzaria Luna
#' @date June 2017, updated March 2019
#' # https://github.com/Azure/doAzureParallel
#wget http://www.omegahat.net/XMLSchema/XMLSchema_0.7-0.tar.gz
#wget http://www.omegahat.net/SSOAP/SSOAP_0.9-0.tar.gz
#install.packages("~/XMLSchema_0.7-0.tar.gz", repos = NULL, type = "source")
#install.packages("~/SSOAP_0.9-0.tar.gz", repos = NULL, type = "source")

#current Atlantis model manual https://research.csiro.au/atlantis/wp-content/uploads/sites/52/2021/01/AtlantisUserGuide_PartI.pdf

# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))

# List of packages for session
.packages = c("XML","RCurl","devtools", "dtplyr","stringi","data.table","tidyverse","stringr","R.utils",
              "magrittr","future","parallel","doSNOW","XMLSchema", "SSOAP","here", "diffr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#give write read permission to whole amps_runs folder

 system("sudo chmod -R a+rwx ~/", wait = TRUE)

 #check file differences
 # diffr("/home/atlantis/psatlantismodel/Atlantis_mv_2/AMPSbioparam_mv2_2022.prm", "/home/atlantis/psatlantismodel/Atlantis_mv_2/AMPSbioparamV6_B1984.prm", contextSize = 3, minJumpSize = 10, wordWrap = TRUE)

 #find string
# grep -rnw 'home/trunk_6644/atlantis' -e '17'
 
# system("sudo apt-get update; sudo apt-get upgrade -y", wait = TRUE)
# system("sudo apt-get install subversion build-essential subversion flip autoconf libnetcdf-dev libxml2-dev libproj-dev -y", wait = TRUE)
#
# system("sudo apt-get update; sudo apt-get upgrade -y; sudo apt-get autoremove -y", wait = TRUE)
# system("sudo dpkg –configure -a -y; sudo apt-get install -f -y", wait = TRUE)
# system("sudo apt-get autoremove") 

 #List revisions
 #svn list -v https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username XXXXX --password XXXXX
 #svnlook history https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username XXXXX --password XXXXX
 
  
# system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username XXXXX --password XXXXX --quiet", wait = TRUE)
# system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/AtlantisSalmon XXXXX --password XXXXX --quiet", wait = TRUE)
# system("svn --no-auth-cache co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/bec_dev -r 6177 --username  XXXXX --password XXXXX --quiet", wait = TRUE)
#system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/AtlantisMig2020 -r 6533 --username XXXXX --password XXXXX --quiet", wait = TRUE)
 
 
# # # Rebuild recompile via MAKE:
# system("cd trunk/atlantis; aclocal; autoheader; autoconf; automake -a; sudo ./configure; sudo make -i CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough -Wno-unused-but-set-variable -Wall -Wno-pedantic'; sudo make -d install", wait = TRUE)
# ./configure --enable-rassesslink #this might work if there is an R problem

 #sudo apt-get install valgrind
#G_SLICE=always-malloc G_DEBUG=gc-friendly  valgrind -v --tool=memcheck --leak-check=full --num-callers=40 --log-file=valgrind.log ./trunk/atlantis/atlantismain/atlantisMerged
#valgrind -q ./trunk/atlantis/atlantismain/atlantisMerged
 
 #valgrind --tool=memcheck --leak-check=yes --show-reachable=yes --num-callers=20 --track-fds=yes ./atlantisMerged
 
# # # copy Atlantis executable
#  system(paste("sudo cp -u trunk/atlantis/atlantismain/atlantisMerged ~/ampsmodelvariants"), wait = TRUE)

#this will create multiple folders each with all Atlantis run files



sh.file <- "amps_cal.sh" #original sh file

folder.paths <- here(c("Atlantis_mv_1"))

folder.length <- 1:length(folder.paths)

#run multiple Atlantis simulations on local machine cores

NumberOfCluster <- detectCores()  #- 1

# Initiate cluster
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

# Run this for loop for one call of model from each cluster, assuming cluster is already initiated. 
atlantis.scenarios <- foreach(this.index=folder.length, .verbose = TRUE) %dopar% {
  
  # List of packages for session
  .packages = c("stringi","data.table","tidyverse","stringr","R.utils")
  
  # Install CRAN packages (if not already installed)
  .inst <- .packages %in% installed.packages()
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  this.folder <- folder.paths[this.index]
  setwd(this.folder)
  system("sudo chmod -R a+rwx ~/", wait = TRUE)
  
  # run Atlantis scenario
  system(paste("sudo flip -uv *; sudo chmod +x ", sh.file,"; sudo sh ./", sh.file, sep=""), wait = TRUE)
  
  done <- as.data.frame("done")
}


stopCluster(cl)

system("az vm deallocate --name summit03 --no-wait --resource-group testingvms")

#file.remove(list.files(path = "~/ampsmodelvariants/Atlantis_B299/outputFolder", pattern = "*.pdf", full.names = TRUE))

