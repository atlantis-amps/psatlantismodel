.packages = c("tidyverse","RNetCDF", "here")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#


this.folder <- "base_runs"
model.var <- 2:8

folder.paths <- here(this.folder,paste0("Atlantis_mv_",model.var,"/outputFolder"))

model.ver <- 1:length(folder.paths)

yearsrun <- 30

theseyears <- 1:yearsrun

max.timestep <- (yearsrun*365)/73 #this is max number of timesteps/73 output frequency


func.groups <- read_csv(here("salmon_groups.csv")) %>% 
  dplyr::rename(group=Name) # get functional groups

salmon.groups <- func.groups %>% 
  pull(group)

get_ages <- function(eachgroup){
  
  this.row <- func.groups %>% 
    filter(group==eachgroup)
  print(this.row)
  
  group.ages <- tibble(agename = paste0(eachgroup,1:this.row$NumCohorts,"_Nums"), group = eachgroup) %>% 
    mutate(age = 1:nrow(.))
   
  
}

salmon.ages <- lapply(salmon.groups, get_ages) %>% 
  bind_rows() %>% 
  left_join(func.groups, by = c("group"))

vert.length <- 1:nrow(salmon.ages)


get_nums <- function(eachagerow, salmon.ages, nc.file){

  thisrow <- salmon.ages[eachagerow,]
  eachvariable <- thisrow$agename
  
  print(eachvariable)
  
  thisData <- var.get.nc(nc.file, eachvariable)
  thisData[thisData==0]<-NA  # Replace 0's with NA
  thisData <- thisData[1:7,2:89,1:max.timestep]
  thisDataNums <-apply(thisData,3,sum,na.rm = TRUE) #Get nums over time, summing over depth and location  
  nums.age <- tibble(nums=thisDataNums, agename = eachvariable) %>% 
    left_join(thisrow, by="agename") %>% 
    mutate(timestep = 1:nrow(.), year = ((timestep *73)/365), year_no = as.integer(2011+year))

    # Normalize by initial value
    
  }

get_nums_runs <- function(eachmodelver, get_nums, vert.length, salmon.ages, folder.paths){
  
  eachfolderpath <- folder.paths[eachmodelver]
  print(eachfolderpath)
  
  nc.file <- open.nc(paste0(eachfolderpath,"/AMPS_OUT.nc")) # open nc output file
  
  salmon.nums <- lapply(vert.length, get_nums, salmon.ages, nc.file) %>% 
    bind_rows %>% 
    mutate(model_ver = eachmodelver)
  
  return(salmon.nums)
  
}
  

salmon.run.nums <- lapply(model.ver,get_nums_runs,get_nums, vert.length, salmon.ages, folder.paths) %>% 
  bind_rows()

write_csv(salmon.run.nums, here("predation_test","ensemble_numbers_age_p.csv"))

ensemblenumbersage <- salmon.run.nums 

plot_ensemble_survival <- function(ensemblenumbersage, salmongroups){ #, plotmodels
  
  salmon.max.nums <- ensemblenumbersage %>%
    dplyr::group_by(model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() #%>%
  #  dplyr::filter(!model_ver%in% plotmodels)
  
  salmon.juv.nums <- salmon.max.nums %>%
    dplyr::filter(age == 1) %>%
    dplyr::rename(juv_nums = max_nums)
  
  salmon.return.nums <- salmon.max.nums %>%
    dplyr::filter(age == (years_away + 1)) %>%
    dplyr::mutate(cohort_yr = year_no - age) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums=max_nums, year_sim = year_no, year_no= cohort_yr) %>%
    dplyr::left_join(salmon.juv.nums, by=c("model_ver","Code","year_no","Long.Name","NumCohorts","Name")) %>%
    dplyr::mutate(survival = return_nums/juv_nums) %>%
    dplyr::mutate(model_ver = as.factor(model_ver)) %>%
    dplyr::mutate(Year = year_sim - 2010)
  
  
  max_year  <- max(salmon.return.nums$year_no)
  
  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    nrow(salmongroups)/ 12
  )
  
  #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")
  
  print(n_pages)
  for (i in seq_len(n_pages)) {
    
    survival.plot <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, group= model_ver, colour=model_ver))+
      ggplot2::geom_line() +
      ggplot2::ylim(0,1) +
      ggthemes::theme_few() +
      ggthemes::scale_colour_few(name = "Availability matrix") +
      ggforce::facet_wrap_paginate(~ Long.Name, ncol = 3, nrow = 4, page = i, shrink = FALSE, labeller = 'label_value')+
      ggplot2::labs(y="Survival")
    
    thisplotname <- paste0("salmon_survival_plot_pp_",i,".png")
    
    ggplot2::ggsave(here(this.folder, thisplotname), plot = survival.plot, device = "png", width = 21, height = 24, units = "cm")
    
  }
  
  
  return(salmon.return.nums)
}

#plotmodels <- c() # eliminated model versions 1 & 6

salmongroups <- read_csv("salmon_groups.csv") %>% 
  dplyr::rename(Long.Name = `Long Name`)

plot_ensemble_survival(salmon.run.nums,salmongroups) #, plotmodels
