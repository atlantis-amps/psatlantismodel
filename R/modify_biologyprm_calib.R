#' Code to generate new biology.prm files
#' Inputs: 2024_calibration_log Google sheets
#' Outputs: Modified biology.prm 
#' @author Hem Nalini Morzaria Luna
#' @date Last edited January 2024
#' 

# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))

# List of packages for session
.packages = c("devtools","dtplyr","stringi","data.table","tidyverse","stringr","R.utils","magrittr",
              "future","parallel","doSNOW","scales","RNetCDF","sf", "here", "googlesheets4")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#this will take a while
system("sudo chmod -R a+rwx ~/", wait = TRUE)

# Authenticate manually
#googlesheets4::gs4_auth()

# Set authentication token to be stored in a folder called `.secrets`
#options(gargle_oauth_cache = ".secrets")

# If successful, the previous step stores a token file.
# Check that a file has been created with:
#list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
googlesheets4::gs4_deauth()

#if getting a Error in `gs4_get_impl_()`:
#! Client error: (400) FAILED_PRECONDITION
#make sure you are reading a google docs and not an .xlsx file

prm.modify <- googlesheets4::read_sheet("1KskR_Jzs33C_1zhSNwAPNCOliM9q6ftD7DSywSYW568", sheet = "entered_values")


#base run, to use biology prm and copy additional directories


#runs that will be modified, based on run_no in the spreadsheet
runs.modify <- 161:176

base.dir <- "/home/atlantis/atlantispssalmon/Atlantis_mv_1_base_Jan2024_10yrs"
biology.prm <- "AMPSbioparam_mv1_2022.prm"

modify_prm <- function(eachrun, base.dir, biology.prm){
 
  #runs that will be created
rows.modify <- prm.modify %>% 
  dplyr::filter(run_no %in% eachrun)

#directory name
dir.modify <- rows.modify %>% 
  dplyr::distinct(run_name) %>% 
  dplyr::pull(run_name)

print(paste("Creating run",dir.modify))

if(!dir.exists(dir.modify)){

  #make new directory
  system(paste0("mkdir ", here::here(dir.modify)), wait = TRUE)
  
  #copy basedir but not subdirectories
  # cp dir1/* dir2 
  system(paste0("cp ", here::here(base.dir),"/* ", here::here(dir.modify)), wait = TRUE)

}

#first change parameters that are not vectors
novector.changes <- rows.modify %>% 
  dplyr::filter(is_vector==0)

for(eachrow in 1:nrow(novector.changes)){
  
  thisrow <- novector.changes[eachrow,]
  
  print(thisrow)
  
  old.param <- paste0(thisrow$parameter_name, "\t",unlist(thisrow$original_prm))
  
  new.param <- paste0(thisrow$parameter_name, "\t",unlist(thisrow$new_prm))
  
  readLines(here::here(dir.modify, biology.prm)) %>% 
    stringr::str_replace(
      pattern = old.param, 
      replace = new.param) %>% 
    writeLines(con = here::here(dir.modify, biology.prm))
  
}

}

lapply(runs.modify, modify_prm, base.dir, biology.prm)