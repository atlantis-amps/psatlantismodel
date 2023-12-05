#' Function to track differences in Atlantis parameter
#' Inputs: Folders with different atlantis parameters, compares *.prm files
#' Outputs: Text file with comparison between two para
#' @author Hem Nalini Morzaria Luna
#' @date Last edited November 2023
#' 

# List of packages for session
.packages = c("diffr", "tidyverse","here")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

oldcodepath <- "dir1"
newcodepath <- "dir2"

old.ver <- "run1"
new.ver <- "run2"

output.txt <- paste0("atlantis_param_comp_", old.ver,"-",new.ver, ".txt")
file.create(output.txt)

system("sudo apt-get install moreutils", wait = TRUE)


compare_code <- function(oldcodepath, newcodepath){
  
 code.files <-  list.files(oldcodepath, pattern = "*.prm$", full.names = FALSE, recursive = TRUE)
 
 cat(paste("Versions compared", new.ver, old.ver), file= output.txt, append = FALSE)
 
 for(eachcodefile in code.files){
   
   print(eachcodefile)
   
   cat("", file = output.txt, append = TRUE, sep="\n")  # Add an empty line for spacing
   
   cat("FILE COMPARED", file= output.txt, append = TRUE)
   cat("", file = output.txt, append = TRUE, sep="\n")  # Add an empty line for spacing
   cat(eachcodefile, file= output.txt, append = TRUE)
   cat("", file = output.txt, append = TRUE, sep="\n")  # Add an empty line for spacing
    
   #system(paste0("diff -u ", paste0(newcodepath,"/", eachcodefile)," ", paste0(oldcodepath,"/",eachcodefile), " >> ", output.txt), wait = TRUE)
  try(system(paste0("diff -a --suppress-common-lines -y ", paste0(newcodepath,"/",eachcodefile)," ", paste0(oldcodepath,"/",eachcodefile), " >> ", output.txt), wait = TRUE))
   cat("", file = output.txt, append = TRUE)  # Add an empty line for spacing
   cat("", file = output.txt, append = TRUE)  # Add an empty line for spacing
 }
  
}


compare_code(oldcodepath, newcodepath)

