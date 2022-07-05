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
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))

# List of packages for session
.packages = c("XML","RCurl","devtools", "dtplyr","stringi","data.table","tidyverse","stringr","R.utils",
              "magrittr","future","parallel","doSNOW","XMLSchema", "SSOAP","here")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#give write read permission to whole amps_runs folder

 system("sudo chmod -R a+rwx ~/", wait = TRUE)
#
# system("sudo apt-get update; sudo apt-get upgrade -y", wait = TRUE)
# system("sudo apt-get install subversion build-essential subversion flip autoconf libnetcdf-dev libxml2-dev libproj-dev -y", wait = TRUE)
#
# system("sudo apt-get update; sudo apt-get upgrade -y; sudo apt-get autoremove -y", wait = TRUE)
# system("sudo dpkg –configure -a -y; sudo apt-get install -f -y", wait = TRUE)
# system("sudo apt-get autoremove") 

# # Get code from CSIRO SVN, if need to change version use r -5468 for example
#  system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username isaac.kaplan.768 --password TrevorBarker84! --quiet", wait = TRUE)
# system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6519 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6541 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6520 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6529 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6536 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
  # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6537 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6538 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6539 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 
 
 # system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
# system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/AtlantisSalmon hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 # system("svn --no-auth-cache co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/bec_dev -r 6177 --username  isaac.kaplan.768 --password TrevorBarker84! --quiet", wait = TRUE)
#system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/AtlantisMig2020 -r 6533 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 
 
# # # Rebuild recompile via MAKE:
# system("cd trunk/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough -Wno-unused-but-set-variable'; sudo make -d install; cd atlantismain", wait = TRUE)
# system("cd AtlantisSalmon; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)
# system("cd trunk_ICKpopecol/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)
 # system("cd trunk_v6536_Nov_2020/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough -Wno-unused-but-set-variable'; sudo make -d install; cd atlantismain", wait = TRUE)
 # system("cd AtlantisMig2020/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough -Wno-unused-but-set-variable'; sudo make -d install; cd atlantismain", wait = TRUE)
# system("sudo chmod -R a+rw ~/trunk")
# system("cd AtlantisMig2020_6533_Oct_2020/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough -Wno-unused-but-set-variable -Wno-unknown-warning-option -Wno-error -Wno-pointer-bool-conversion'; sudo make -d install; cd atlantismain", wait = TRUE)
 
# system("cd trunk/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-implicit-fallthrough -Wno-unused-but-set-variable -Wno-unknown-warning-option -Wno-error -Wno-pointer-bool-conversion'; make -d install; cd atlantismain", wait = TRUE)
# system("cd AtlantisMig2020/atlantis; aclocal; autoheader; autoconf; automake -a; sudo ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough -Wno-unused-but-set-variable -Wno-error -Wno-pointer-bool-conversion'; sudo make -d install; cd atlantismain", wait = TRUE)
 
 
 #sudo apt-get install valgrind
#G_SLICE=always-malloc G_DEBUG=gc-friendly  valgrind -v --tool=memcheck --leak-check=full --num-callers=40 --log-file=valgrind.log ./trunk/atlantis/atlantismain/atlantisMerged
 
# # # copy Atlantis executable
#  system(paste("sudo cp -u trunk/atlantis/atlantismain/atlantisMerged ~/ampsmodelvariants"), wait = TRUE)

#this will create multiple folders each with all Atlantis run files

no.folders <- c(1:8)

folder.length <- 1:length(no.folders)

sh.file <- "amps_cal.sh" #original sh file

folder.paths <- here("base_runs",paste0("Atlantis_mv_",no.folders))

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



system("az vm deallocate --name atlantisgroupvm04 --no-wait --resource-group atlantisgroup")

#file.remove(list.files(path = "~/ampsmodelvariants/Atlantis_B299/outputFolder", pattern = "*.pdf", full.names = TRUE))

