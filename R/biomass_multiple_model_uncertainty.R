#' @title Code to plot multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna
#' @date February 2021
#' 

system("sudo chmod -R a+rw /usr/local/lib/R/site-library")

# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

# List of packages for session
.packages = c("devtools","dtplyr","stringi","data.table","tidyverse","stringr",
              "here","paletteer","Redmonder","ggforce")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

system("sudo chmod -R a+rw ~/")


this.folder <- "base_runs"
model.var <- c(1:8)

folder.paths <- here(this.folder,paste0("Atlantis_mv_",model.var,"/outputFolder"))

model.ver <- 1:length(folder.paths)

years.run <- 20

this.timestep <- (years.run*365)/73 #this is max number of timesteps/73 output frequency

max.timestep <- years.run*365


fg.list <- read_csv(here('PugetSoundAtlantisFunctionalGroups.csv')) %>% 
  dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname)

variabletypes <- c("BoxBiomass","Biomass","StructN","ResN","Nums","Wage")
#variabletypes <- c("BoxBiomass","Biomass","StructN","ResN","Nums","Wage")

#use for NPZ
#variabletypes <- c("BoxBiomass","Biomass")

vert.groups <- fg.list %>% 
  filter(IsTurnedOn==1) %>% 
  filter(GroupType == "FISH" | GroupType == "SHARK" | GroupType == "BIRD"| GroupType == "MAMMAL") %>% 
  dplyr::select(name) %>% .$name


get_biomass <- function(thismodelver, folder.paths, fg.list, max.timestep){

  thisfolder <- folder.paths[thismodelver]
  
  print(thisfolder)
  
  this.output.biomass <- read_delim(paste0(thisfolder,"/AMPS_OUTBiomIndx.txt"), delim = " ") %>% 
    dplyr::select(Time:DIN) %>% 
    gather(Code,biomass, -Time) %>% 
    left_join(fg.list, by="Code") %>% 
    filter(Time <= max.timestep) %>% 
    mutate(Year = Time/365) %>% 
    dplyr::select(-IsTurnedOn, -GroupType, -NumCohorts, -Time) %>% 
    mutate(model_ver = thismodelver)
  
  return(this.output.biomass)
  
}

model.biomass <- lapply(model.ver, get_biomass, folder.paths, fg.list, max.timestep)


model.biomass.frame <- model.biomass %>% 
  bind_rows %>% 
  mutate(longname = as.factor(longname))%>% 
  mutate(model_ver = as.factor(model_ver)) %>% 
  mutate(longname=gsub("Subyearling","Subyrlng",longname)) %>% 
  mutate(longname=gsub("Yearling","Yrlng",longname))
 
model.biomass.frame %>% 
  write_csv("ensemble_biomass.csv")

plot.biomass <- model.biomass.frame %>% 
  filter(longname!="Carrion") 
  
thisvariabletype <- "Biomass"
#library(paletteer) 
#paletteer_d("Redmonder::qMSOSlp")

col.pal <- redmonder.pal(8, "qMSOSlp")


  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    length(levels(as.factor(model.biomass.frame$longname)))/ 16
  )
  
  print(n_pages)
  for (i in seq_len(n_pages)) {
    
    print(i)
    
    pplot <-  ggplot(plot.biomass, aes(x=Year,y=biomass, group = model_ver, colour = model_ver))+
      geom_line()+
      labs(y= thisvariabletype, x = "Year") +
      ggthemes::theme_few() +
      ggthemes::scale_colour_few(name = "Availability matrix") +
      #scale_color_manual(values=col.pal, name = "Model version")+
      scale_y_continuous(limits = c(0,NA))+
      facet_wrap_paginate(~longname, ncol = 4, nrow = 4, page = i, shrink = FALSE, scales = "free")+
      theme_minimal()+
      theme(strip.text = element_text(size = 7),
            legend.position="bottom",
            axis.text.x = element_text(size =c(8)),
            axis.text.y = element_text(size =c(8)))
    
      
    
    
    thisplotname <- paste(thisvariabletype,i,"model_comparison_plot.pdf",sep="_")
    
    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggsave(here(this.folder,thisplotname),plot = pplot, width = 21, height = 29, units = "cm")
  }
