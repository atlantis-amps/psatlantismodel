#' Code to compare biomass output from Atlantis simulations
#' Inputs: BiomIndx.txt
#' Outputs: Plots of biomass; 
#' Outputs: csv files of biomass
#' @author Hem Nalini Morzaria-Luna
#' @date Last edited October 2023

# List of packages for session
.packages = c("devtools","tidyverse", "here")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

system("sudo chmod -R a+rw ~/")

system("sudo chmod -R a+rw /usr/local/lib/R/site-library")

##CHANGE THESE PARAMETERS
#this.folder <- "base_runs" # 
model.var <- c(1) # different model versions that are used

#change folder name here
folder.paths <- c(here::here("dir1", "outputFolder"), 
                  here::here("dir2", "outputFolder"),
                  here::here("dir3","outputFolder"))

#specify output frequency and years run
model.ver <- 1:length(folder.paths)
outputfrequency <- 73 #1 day output frequency
yearsrun <- 5

theseyears <- 1:yearsrun

thistimestep <- (yearsrun*365)/outputfrequency #this is max number of timesteps/73 output frequency

maxtimestep <- yearsrun*365

#name ouput file
biom.output.file <- "biom_comp_6693_base_mv1"
#add run names
run.names <- c("6693","6690","6681","6676","6533M")

##END TO CHANGES IN PARAMETERS


#specify colors
run.colors <- c("#b30000", "#7c1158", "#4421af", "#1a53ff", "#0d88e6", "#00b7c7", "#5ad45a", "#8be04e", "#ebdc78",
                                   "#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6")

names(run.colors) <- run.names


if(file.exists(here::here(paste0(biom.output.file,".csv")))==TRUE){
  
  file.remove(here::here(paste0(biom.output.file,".csv")))
}

dir.create(here::here("comparison_plots"))

fg.list <- read_csv(here('PugetSoundAtlantisFunctionalGroups.csv')) %>% 
  dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>% 
  filter(!Code %in% c("DIN","DL"))



read_biomass <- function(eachfolder, fg.list, folder.paths, run.names) {
  
this.folder <- folder.paths[eachfolder] 
this.run <- run.names[eachfolder]
  
  this.output.biomass <- readr::read_delim(paste0(this.folder,"/","AMPS_OUTBiomIndx.txt")) %>% 
    dplyr::select(Time:DIN) %>% 
    tidyr::pivot_longer(cols =BB:DIN, names_to= "Code",values_to="biomass") %>% 
    dplyr::left_join(fg.list, by="Code") %>% 
    dplyr::filter(Time <= maxtimestep) %>% 
    dplyr::mutate(Year = Time/365) %>% 
    dplyr::select(-IsTurnedOn, -GroupType, -NumCohorts, -Time)  %>% 
    dplyr::mutate(run_name = this.run)
  
  if(eachfolder==1) write_csv(this.output.biomass, here::here(paste0(biom.output.file,".csv")), append = FALSE)
  if(eachfolder!=1) write_csv(this.output.biomass, here::here(paste0(biom.output.file,".csv")), append = TRUE)
  
  
}

plot_biomass <- function(biom.output.file){
  
  thisdataset <- data.table::fread(here::here(paste0(biom.output.file,".csv")))
  
  
  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    length(levels(as.factor(thisdataset$longname)))/ 9
  )
  
  print(n_pages)
  for (i in seq_len(n_pages)) {
    
    print(i)
    
    pplot <-   thisdataset %>% 
    dplyr::mutate(Year = as.factor(Year)) %>% 
    ggplot(aes(x=Year,y=biomass, group = run_name, colour = run_name))+
    geom_line(alpha = 0.6)+
    labs(y= "Biomass", x = "Year") +
    scale_color_manual(values=run.colors) +
    # scale_y_continuous(limits = c(0,NA))+
    ggforce::facet_wrap_paginate(~longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, scales = "free_y")+
    theme(legend.position="none")+
    theme_minimal()
  
  thisplotname <- paste(biom.output.file, i,"plot.pdf",sep="_")
  
  #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
  ggsave(thisplotname, plot = pplot, path = here::here("comparison_plots"), width = 21, height = 29, units = "cm")
  
  }
    
}
  
lapply(model.ver, read_biomass, fg.list, folder.paths, run.names)
plot_biomass(biom.output.file)


