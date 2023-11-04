#' Function to track differences in Atlantis code versions
#' Inputs: Folders with different atlantis code versions, compares *.c files
#' Outputs: text file with differences in files
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

old.ver <- "codeversion1"
new.ver <- "codeversion2"

output.txt <- paste0("atlantis_code_comp_",old.ver,"-",new.ver, ".txt")
file.create(output.txt)

compare_code <- function(oldcodepath, newcodepath){
  
 code.files <-  list.files(newcodepath, pattern = "*.c$", full.names = FALSE, recursive = TRUE)
 
 system("sudo apt-get install moreutils", wait = TRUE)
 
 cat(paste("Versions compared", new.ver, old.ver), file= output.txt, append = FALSE)
 
 for(eachcodefile in code.files){
   
   print(eachcodefile)
   
   cat("", file = output.txt, append = TRUE, sep="\n")  # Add an empty line for spacing
   
   cat("FILE COMPARED", file= output.txt, append = TRUE)
   cat("", file = output.txt, append = TRUE, sep="\n")  # Add an empty line for spacing
   cat(eachcodefile, file= output.txt, append = TRUE)
   cat("", file = output.txt, append = TRUE, sep="\n")  # Add an empty line for spacing
    
   #system(paste0("diff -u ", paste0(newcodepath,"/", eachcodefile)," ", paste0(oldcodepath,"/",eachcodefile), " >> ", output.txt), wait = TRUE)
   system(paste0("diff -a --suppress-common-lines -y ", paste0(newcodepath,"/",eachcodefile)," ", paste0(oldcodepath,"/",eachcodefile), " >> ", output.txt), wait = TRUE)
   cat("", file = output.txt, append = TRUE)  # Add an empty line for spacing
   cat("", file = output.txt, append = TRUE)  # Add an empty line for spacing
 }
  
}


compare_code(oldcodepath, newcodepath)

#To further compare specific files using the GUI
file.new <- paste0(newcodepath,"/atecology/atdemography.c")
file.old <- paste0(oldcodepath,"/atecology/atdemography.c")

library("diffr")

diffr(file1 = file.old, file2 = file.new, contextSize = 3, minJumpSize = 10, wordWrap = TRUE, width = NULL, height = NULL)
