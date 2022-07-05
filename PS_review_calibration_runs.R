#' Code to manage and run Atlantis simulations
#'  
#' @author Hem Nalini Morzaria Luna
#' @date February 2019
#' 

# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))

# List of packages for session
.packages = c("devtools","dtplyr","stringi","data.table","tidyverse","stringr","R.utils","magrittr",
              "future","parallel","doSNOW","ReactiveAtlantis","scales","RNetCDF","ggforce","pdftools",
              "rgdal","gridExtra", "esquisse","sf")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#install_github('Atlantis-Ecosystem-Model/ReactiveAtlantis', force=TRUE, dependencies=TRUE)


#cp -r /home/atlantis/ampsmodelvariants/Atlantis_B331 /home/atlantis/ampsmodelvariants/Atlantis_B332

system("sudo chmod -R a+rw ~/")

system("sudo chmod -R a+rw /usr/local/lib/R/site-library")


this.folder <- "base_runs"
model.var <- c(2:4,7,8)

folder.paths <- here(this.folder,paste0("Atlantis_mv_",model.var,"/outputFolder"))

model.ver <- 1:length(folder.paths)

yearsrun <- 30

theseyears <- 1:yearsrun

thistimestep <- (yearsrun*365)/73 #this is max number of timesteps/73 output frequency

maxtimestep <- yearsrun*365



add_file <- function(eachfolder, this.file){
  
  
  file.copy(from=here(this.file), to = paste0(eachfolder,"/",this.file), overwrite = TRUE)
  
 
}

lapply(folder.paths, add_file, this.file = "PugetSoundAtlantisFunctionalGroupsOriginal.csv")
lapply(folder.paths, add_file, this.file = "PugetSoundAtlantisFunctionalGroups.csv")
lapply(folder.paths, add_file, this.file = "PugetSound_89b_070116.bgm")


# make detailed Biomass, biomass per plot, Rn Sn, and Nums plots

fg.list <- read_csv(here('PugetSoundAtlantisFunctionalGroups.csv')) %>% 
  dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname)

variabletypes <- c("BoxBiomass","Biomass","StructN","ResN","Nums","Wage")
#variabletypes <- c("BoxBiomass","Biomass","StructN","ResN","Nums","Wage")

#use for NPZ
#variabletypes <- c("BoxBiomass","Biomass")

outputfolder.name <- "outputFolder"

vert.groups <- fg.list %>% 
  filter(IsTurnedOn==1) %>% 
  filter(GroupType == "FISH" | GroupType == "SHARK" | GroupType == "BIRD"| GroupType == "MAMMAL") %>% 
  dplyr::select(name) %>% .$name

#vert.groups <- c("Herring_PS","Sm_Plank_Fish","Herring_Cherry","SmDem_Fish","Perch_Fish","SmFlat_Fish","PinkSY_Fish")


# vert.groups <- c("Herring_PS","Sm_Plank_Fish","Herring_Cherry","SmDem_Fish","Perch_Fish","SmFlat_Fish","PinkSY_Fish","SGeorgia_Fish","OtherSal_Fish","ChumHSY_Fish","ChumFSY_Fish","ChumHCSY_Fish",
#                  "CohoHY_Fish","CohoSY_Fish","CohoDSY_Fish","CohoOY_Fish","ChinookY_Fish","ChinookSY_Fish","ChinookSKY_Fish",    
#                  "ChinookSKSY_Fish","ChinookSSY_Fish", "ChinookDSY_Fish", "ChinookNY_Fish","ChinookNSY_Fish", "ChinookHY_Fish","ChinookOY_Fish", 
#                  "ChinookOSY_Fish","DemRock_Fish", "MidRock_Fish","DemVRock_Fish","MidVRock_Fish",
#                  "Gadiods_Fish","LgDem_Fish","PiscFlat_Fish","Spinydog_Fish","Sixgill_Shark",
#                  "SkateRay","Ratfish")
# 


                   
shape.file <- readOGR(dsn=here(), layer="bgm_Puget_Sound_89b_0p0001_WGS84", stringsAsFactors = FALSE)
 
#shape.file <- st_read("~/ampsmodelvariants/bgm_Puget_Sound_89b_0p0001_WGS84.shp")

shape.file.df <- fortify(shape.file, region = "BOX_ID") %>% 
   mutate(id=as.numeric(id))

#get values from NC file
get.nc.data <- function(eachgroup,thisncfile){
  
  print(paste("Analyzing this group",eachgroup))
  
  this.sprow <- fg.list %>% 
    filter(name==eachgroup) 
  
  print(this.sprow)
  
  group.ages <- paste(eachgroup,1:this.sprow$NumCohorts,sep="")
  
  print(group.ages)
  
  #make names for nc variables
  
  varlist <- c("_ResN","_Nums","_StructN","_Wage")
  
  var.listdata <- list()
  
  for(eachvar in 1:length(varlist)){
    
    eachvarlist <- varlist[eachvar]
    
    print(eachvarlist)
    
    name.var <- paste(group.ages,eachvarlist,sep="")
    
    variable.type <- gsub("_","",eachvarlist)
    
    if(eachvarlist == "_ResN" | eachvarlist == "_StructN" | eachvarlist == "_Wage") {
      
      for(eachage in 1:length(name.var)) {
        
        if(eachvarlist == "_Wage"){
          
          eachvarlist = "_ResN"
          name.var <- paste(group.ages,eachvarlist,sep="")
          variable.type <- gsub("_","",eachvarlist)
          eachvarlist = "_Wage"
        }
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- var.get.nc(thisncfile, eachvariable)
        
        if(eachvarlist == "_Wage") {
          
          thisData<-thisData*20*5.7*(3.65/2.65)/1000000
          
          variable.type = "Wage"
        }
        
        thisData[thisData==0]<-NA  # Replace 0's with NA
        thisData <- thisData[1:7,2:89,1:thistimestep]
        thisDataMeanMg <-apply(thisData,3,mean,na.rm = TRUE) #Get mean size over time, averaging over depth and location 
        
        thisY <-tibble(variable=thisDataMeanMg/thisDataMeanMg[1]) %>%  # Normalize by initial value
          mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
        
        if(eachvarlist == "_Wage") {
          
          thisY <-tibble(variable=thisDataMeanMg) %>%  # Normalize by initial value
            mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
          
        }
        
        listname <- paste(thisY$group[1],"_",thisY$variable_type[1],sep="")
        var.listdata[[listname]] <- thisY  
        
      } 
      
    } else if (eachvarlist == "_Nums") {
      
      for(eachage in 1:length(name.var)) {
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- var.get.nc(thisncfile, eachvariable)
        thisData[thisData==0]<-NA  # Replace 0's with NA
        print(dim(thisData))
        thisData <- thisData[1:7,2:89,1:thistimestep]
        #thisData <- thisData[1:7,2:89,1:51] #use this for 10 year runs
        thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location  
        thisY <-tibble(variable=thisDataNums) %>%  # Normalize by initial value
          mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
        
        var.listdata[[eachvariable]] <- thisY  
        
      }
      
    }
  }
  
  
  thissp.data <- var.listdata %>% 
    bind_rows() %>% 
    mutate(code = this.sprow$Code, longname = this.sprow$longname) %>% 
    dplyr::rename(atlantis_group = group) %>% 
    mutate(Year=(time*73)/365)
  
  
  print(paste("Done with group",eachgroup))
  
  
  return(thissp.data)
}

#make graph with output from NC file
make_graph <- function(thisvariabletype){
  
  print(thisvariabletype)
  
  if(thisvariabletype=="Biomass"){
    
    thisdataset <- this.output.biomass %>% 
      mutate(longname = as.factor(longname))
    
    # Calculate the number of pages with 12 panels per page
    n_pages <- ceiling(
      length(levels(as.factor(thisdataset$longname)))/ 12
    )
    
    print(n_pages)
    for (i in seq_len(n_pages)) {
      
      print(i)
      
      pplot <-   ggplot(thisdataset, aes(x=Year,y=biomass, group = longname))+
        geom_line(colour="darkblue")+
        labs(y= thisvariabletype, x = "Year") +
        scale_y_continuous(limits = c(0,NA))+
        facet_wrap_paginate(~longname, ncol = 3, nrow = 4, page = i, shrink = FALSE, scales = "free")+
        theme(legend.position="none")+
        theme_minimal()
        
        
      
      thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")
      
      #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
      ggsave(thisplotname, plot = pplot, path = eachfolder, width = 21, height = 29, units = "cm")
    }
  } else if(thisvariabletype=="BoxBiomass"){

    thisdataset <- this.output.box.biomass.df %>% 
      mutate(longname = as.factor(longname))

      species.list <- thisdataset %>% 
        distinct(longname) %>% 
        mutate(longname=as.character(longname)) %>% 
        .$longname
      
      p <- list()
      
      for(eachspecies in 1:length(species.list)){
        
        print(species.list[eachspecies])
        
        sp.dataset <- thisdataset %>% 
          filter(longname==species.list[eachspecies])
        
          p[[eachspecies]] <- ggplot(data = sp.dataset, aes(x = long, y = lat, group = group))+ 
            geom_polygon(aes(fill = biomass), color = 'gray', size = 0.1) +
            coord_fixed(1.3)+
            theme_minimal()+
            ggtitle(species.list[eachspecies])
      }
      
      # Calculate the number of pages with 9 panels per page
      n_pages <- ceiling(
        length(levels(as.factor(thisdataset$longname)))/ 9
      )
      
      print(n_pages)
      
      for (i in seq_len((n_pages-1))) {
      print(i)  
      thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")
      print(thisplotname)
      pdf(thisplotname, width = 12, height = 12)
      
      if(i == 1){do.call(grid.arrange,p[(i):(9*i)])} else {
      
        do.call(grid.arrange,p[((i*9)-8):(9*i)])
      }
      
      dev.off()
      
      }
  } else if (thisvariabletype=="StructN" | thisvariabletype=="ResN") {
    
    thisdataset <- group.atlantis.data %>% 
      filter(variable_type==thisvariabletype) %>% 
      mutate(age = as.factor(age))
    
    # Calculate the number of pages with 12 panels per page
    n_pages <- ceiling(
      length(levels(as.factor(thisdataset$longname)))/ 12
    )
    
    print(n_pages)
    
    for (i in seq_len(n_pages)) {
      print(i)
      pplot <-   ggplot(thisdataset, aes(x=Year,y=variable, group = age))+
        geom_line(aes(colour= age))+
        labs(y= thisvariabletype, x = "Year") +
        # facet_wrap(~longname, scales = "free")
        facet_wrap_paginate(~longname, ncol = 3, nrow = 4, page = i, scales = "free")+
        theme(legend.position="none")+
        geom_hline(yintercept=1, linetype="solid", color = "black")+
        geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
        geom_hline(yintercept=1.5, linetype="dashed", color = "red")
      
      thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")
      
      # ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
      ggsave(thisplotname, plot = pplot, path = eachfolder, width = 21, height = 29, units = "cm")
    }
    
  }  else {
    
    thisdataset <- group.atlantis.data %>% 
      filter(variable_type==thisvariabletype) %>% 
      mutate(age = as.factor(age))
    
    # Calculate the number of pages with 12 panels per page
    n_pages <- ceiling(
      length(levels(as.factor(thisdataset$longname)))/ 12
    )
    
    print(n_pages)
    
    for (i in seq_len(n_pages)) {
      print(i)
      pplot <-   ggplot(thisdataset, aes(x=Year,y=variable, group = age))+
        geom_line(aes(colour= age))+
        labs(y= thisvariabletype, x = "Year") +
        # facet_wrap(~longname, scales = "free")
        facet_wrap_paginate(~longname, ncol = 3, nrow = 4, page = i, scales = "free")+
        theme(legend.position="none")
      
      thisplotname <- paste(thisvariabletype,i,"plot.pdf",sep="_")
      
      # ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
      ggsave(thisplotname, plot = pplot, path=eachfolder, width = 21, height = 29, units = "cm")
    }
  }
  
  
}


for(eachfolder in folder.paths) {
  
  
  setwd("~/")
  print(eachfolder)
  
  this.output.nc <- "AMPS_OUT.nc"
  
  this.output.biomass <- read_delim(paste0(eachfolder,"/AMPS_OUTBiomIndx.txt"), delim = " ") %>% 
    dplyr::select(Time:DIN) %>% 
    gather(Code,biomass, -Time) %>% 
    left_join(fg.list, by="Code") %>% 
    filter(Time <= maxtimestep) %>% 
    mutate(Year = Time/365) %>% 
    dplyr::select(-IsTurnedOn, -GroupType, -NumCohorts, -Time) 
  
  this.output.box.biomass <- read_delim(paste0(eachfolder,"/AMPS_OUTBoxBiomass.txt"), delim = " ") %>%
    gather(Code,biomass, -Time, -Box) %>%
    left_join(fg.list, by="Code") %>%
    mutate(Year = Time/365) %>% 
    filter(Time <= maxtimestep)
  
  max.year <- this.output.box.biomass %>% pull(Year) %>% max
    
  this.output.box.biomass.df <-  this.output.box.biomass %>% 
    filter(Year == max.year) %>%
    dplyr::select(-Code, -Time, -GroupType, -IsTurnedOn, -NumCohorts, -name) %>%
    dplyr::rename(id=Box) %>% 
    left_join(shape.file.df, by="id")
  
 ggplot() +
   geom_polygon(data = this.output.box.biomass.df, aes( x = long, y = lat, group = group, fill=biomass, color=biomass)) +
   theme_void() +
 facet_wrap(~ longname)

  nc <- open.nc(paste0(eachfolder,"/",this.output.nc))
  nc.data <- read.nc(nc)
  
  group.atlantis.data <- lapply(vert.groups, get.nc.data, thisncfile = nc) %>% 
    bind_rows()
  
 print("Graphing output")
lapply(variabletypes, make_graph)
   
   
list.files(pattern="*.*pdf") %>% 
pdf_combine(., output = paste0(eachfolder,"/run_output.pdf"))
  
}


#system("az vm deallocate --name atlantisserver04 --no-wait --resource-group morzariacedogroup")

for(eachfolder in folder.paths) {
  
  try(rstudioapi::viewer(paste0(eachfolder,"/run_output.pdf")))
    
}

# 
# for(eachsimulation in no.folders) {
# 
#   print(eachsimulation)
#     system(paste("sudo rm -R /home/atlantis/ampsmodelvariants/Atlantis_B",eachsimulation,"/outputFolder",sep=""))
#     system(paste("sudo rm /home/atlantis/ampsmodelvariants/Atlantis_B",eachsimulation,"/outamps",sep=""))
# 
# 
#  }

group.atlantis.data %>% 
  filter(code=="POP") %>% 
  filter(age==1) %>% 
  filter(variable_type=="Nums") %>% 
  ggplot(aes(x=Year, y=variable, fill=time)) +
  geom_bar(stat="identity")+
  ggtitle("Resident Orca")

#use this to see specific age classes
group.atlantis.data %>% 
  filter(code=="HUW") %>% 
  filter(age==1) %>% 
  filter(variable_type=="Nums") %>% 
  ggplot(aes(x=Year, y=variable, fill=time)) +
  geom_bar(stat="identity")+
  ggtitle("Humpback whale")

group.atlantis.data %>% 
  filter(code=="TOR") %>% 
  filter(age==1) %>% 
  filter(variable_type=="Nums") %>% 
  ggplot(aes(x=Year, y=variable, fill=time)) +
  geom_bar(stat="identity")+
  ggtitle("Transient Orcas")

group.atlantis.data %>% 
  filter(code=="CSL") %>% 
  filter(age==1) %>% 
  filter(variable_type=="Nums") %>% 
  ggplot(aes(x=Year, y=variable, fill=time)) +
  geom_bar(stat="identity")+
  ggtitle("California Sea lions")

group.atlantis.data %>% 
  filter(code=="PIN") %>% 
  filter(age==1) %>% 
  filter(variable_type=="Nums") %>% 
  ggplot(aes(x=Year, y=variable, fill=time)) +
  geom_bar(stat="identity")


#Biomass
#Before running each output folder has to have a copy of the bgm and group.csv files 

# run_path <- paste(work_path,"/",this.run,"/outputFolder",sep="")
# setwd(run_path)
# this.run <- "Atlantis_B114"
# 
# nc.current  <- 'AMPS_OUT.nc'
# nc.old      <- 'AMPS_OUT.nc'
# grp.csv     <- 'PugetSoundAtlantisFunctionalGroupsOriginal.csv'
# bgm.file    <- 'PugetSound_89b_070116.bgm'
# cum.depths  <- c(0,5,25,50,100,150,350) ## This should be the cummulative depth of your model
# ## individual file
# compare(nc.current, nc.out.old = NULL, grp.csv, bgm.file, cum.depths)
# ## compare to previous run
# #compare(nc.current, nc.old, grp.csv, bgm.file, cum.depths)
# 
# 
# 
# 
# #Analyze diet check to find max predators for a prey
# #Specify groups to check and timstep to read
# 
# time.step <-  3650 #in days
# prey.species <- c("SMD")
# #prey.species <- c("CHY","CSY","CSS","CSN","CDS","CSN","CMH","CNY","CHY","CNS","CHC","CYE","CKS","COH","SAL","SAF","PIS")
# #prey.species <- c("BFF","BG","BIV","BC","BD","ZL","ZM")
# 
# diet.check <- read_delim("AMPS_OUTDietCheck.txt",delim=" ")
# 
# diet.frame <- diet.check %>%
#   filter(Time==time.step) %>%
#   gather(prey,diet_value, -Time, -Predator, -Cohort, -Stock, -Updated)
# 
# max.value <- diet.frame %>%
#   group_by(prey) %>%
#   summarise(max_diet = max(diet_value))
# 
# max.predator <- diet.frame %>%
#   left_join(max.value, by="prey") %>%
#   mutate(max_predator=if_else(diet_value==max_diet,"max_predator","none")) %>%
#   filter(max_predator=="max_predator")
# 
# max.predator %>%
#   filter(prey %in% prey.species)
# 
# diet.frame %>%
#   filter(prey %in% prey.species) %>%
#   filter(diet_value!=0) %>%
#   arrange(prey,diet_value) %>% View()
# 


# for(this.index in folder.length){
#   
#   system("sudo chmod -R a+rwx ~/ampsmodelvariants", wait = TRUE)
#   
#   this.folder <- folder.paths[this.index]
#   setwd(paste(workpath,"/",this.folder,sep=""))
#   
#   source("PlotOutNC_NandRNandNumsSurv_NoBox0.r")  # Adds a plot of survival 
#   
#   source("PlotDiets.r")
#   source("PlotDietsOneSpeciesOneCohort.r")
#   
#   
#   
#   
#   setwd("outputFolder")
#   
#   this.out.folder <- paste(paste(workpath,"/",this.folder,"/","outputFolder/",sep=""))
#   PlotOutNC_NandRNandNumsSurv_NoBox0(this.out.folder,2)  # "2 here is to omit box 1, boundary box that sometimes holds migrators
#   #PlotOutNC_NandRNandNums_NoBox0(this.out.folder,2)  # "2 here is to omit box 1, boundary box that sometimes holds migrators
#   
#   # PlotOutNC_NandRNandNums_SiNO3NH3(this.out.folder)
#   #PlotDiets()
#   
#   ###Plot_LN_Nums_NoBox0(this.out.folder,2)  # "2 here is to omit box 1, boundary box that sometimes holds migrators
#   
#   
# }


# diet.frame %>%
#   filter(prey %in% prey.species) %>%
#   filter(diet_value!=0) %>%
#   arrange(desc(diet_value,prey,predator)) %>% esquisse::esquisser()
#   
  
# folder_path <- paste(work_path,"/",this.run,sep="")
# setwd(folder_path)
# 
# 
# grp.csv     <- 'PugetSoundAtlantisFunctionalGroups.csv'
# prm.file    <- 'AMPSbioparamV1.prm'
# diet.file   <- 'AMPS_OUTDietCheck.txt'
# food.web(diet.file, grp.csv)
# # 
# # 
# # #Exploring predator-prey interactions from the initial conditions
# prm.file    <- 'AMPSbioparamV1.prm'
# nc.initial  <- 'AMPS.nc'
# grp.file     <- 'PugetSoundAtlantisFunctionalGroups.csv'
# bgm.file    <- 'PugetSound_89b_070116.bgm'
# cum.depths  <- c(0,5,25,50,100,150,350) ## This should be the cummulative depth of your model
# feeding.mat(prm.file, grp.file, nc.initial, bgm.file, cum.depths)
# # 
# #Predation analysis from the Atlantis output
# biom        <- 'AMPS_OUTBiomIndx.txt'
# diet.file   <- 'AMPS_OUTDietCheck.txt'
# bio.age     <- 'your_AgeBiomIndx.txt' ## optional file. just if you want to check the predation by age
# grp.csv     <- 'PugetSoundAtlantisFunctionalGroups.csv'
# ## Predation by Age
# #predation(biom, grp.csv, diet.file, bio.age)
# ## No predation by Age
# predation(biom, grp.csv, diet.file, age.biomass = NULL)
