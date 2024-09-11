# List of packages for session
.packages = c("stringi","data.table","tidyverse","stringr","R.utils",
              "magrittr","here")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

dir.create(here("figures"))

sim.years <- 5
#use this to subset the years shown
start.year <- 1
end.year <- 5

this.folder <- "Atlantis_mv_1_6681_5yr_fprintfsp_rectype4_out1dy"
#folder.paths <- here(c("Atlantis_mv_1_6678_10yr_fprintfsp_rectype4_out1dy", "Atlantis_mv_1_6671_10yr_fprintfsp_rectype4_out10dy"))


nums.csv <- read_csv(here(this.folder,"outputFolder","Nums.csv"))

#MigID = YrRun = floor(time/365)

migration.array <- read_delim(here(this.folder,"outputFolder","AMPS_OUTMigrationArray.txt")) %>% 
  mutate(year = floor(Time/365))

migration.table <- read_csv("PugetMigrations.csv") # Modified for CSN testing
migration.lifehistory <- read_csv("life_history_params.csv")
migiobox <- read_csv("migiobox.csv")

species.code <- migration.table %>% 
  distinct(GroupCode) %>% 
  pull(GroupCode)

num.sp <- 1:length(species.code)

plot_migration <- function(eachspecies, migration.array, nums.csv, species.code, migration.lifehistory, migration.table, sim.years, this.folder){
 
  print(this.folder)
  thiscode <- species.code[eachspecies]
  thisname <- migration.lifehistory %>% 
    filter(code==thiscode) %>% 
    pull(long_name)
 
  print(thiscode) 
  print(thisname)
  
  thismigrationjuv <- migration.table %>% 
    filter(GroupCode==thiscode) %>% 
    filter(StartStage==0)
  
  thismigrationad <- migration.table %>% 
    filter(GroupCode==thiscode)%>% 
    filter(StartStage==1)
  
  thislifehistory <- migration.lifehistory %>% 
    filter(code==thiscode)
  
  thismigiobox <- migiobox %>% 
    filter(code==thiscode) %>% 
    pull(migiobox)
  
  marray.nums <- migration.array %>% 
    filter(Species==thiscode) %>% 
    mutate(age = Cohort +1) #%>% 
    # dplyr::rename(time = Time, code = Species) %>% 
    # mutate(year_sim = 1:(code.cohort * 28 * 1851),
    #        day_year = rep(1:365, each = (code.cohort * 28), times = (259140/((365)*(code.cohort * 28)))),
    #        age = as.factor(age))
    # 
  code.cohort <- marray.nums %>% 
    distinct(Cohort) %>% 
    pull(Cohort) %>% 
    length
  
  if(nrow(marray.nums)>0){
    
  these.years <- start.year:end.year
    
  sp.nums <- nums.csv %>% 
    filter(code==thiscode)  %>% 
    mutate(year_sim = rep(1:sim.years, each = 365, times = code.cohort),
           day_year = rep(1:365,each = 1, times = (sim.years*code.cohort)),
           age = as.factor(age)) %>% 
    filter(year_sim %in% these.years)
  
    
  
  #write_csv(sp.nums, here(this.folder,"outputFolder",paste0(thiscode,"_nums.csv")))
  
  if(code.cohort==5){
  
      col.pal <- c("skyblue3","steelblue2","deepskyblue1","royalblue1","navy")
    
  } else if(code.cohort==10) {
    
    col.pal <- c("skyblue2","skyblue3","steelblue2","steelblue3","deepskyblue1","deepskyblue3","deepskyblue4","royalblue1","royalblue3","navy")
    
  } else {
    
    col.pal <- c("skyblue3","steelblue2","steelblue3","royalblue1","royalblue3","steelblue4","dodgerblue4","navy")
    
  }
  
  sp.nums.plot.in <- sp.nums %>% 
       ggplot() +
   # geom_line(aes(x= day_year, y = den, colour = age))+
    geom_line(aes(x= day_year, y = variable, colour = age), linetype = "dotted", size = 0.8) +
   # geom_line(aes(x= day_year, y = maxden, colour = age), linetype = "dashed")+
    scale_color_manual(values=col.pal) +
    facet_wrap(year_sim ~ age, nrow= sim.years, ncol = code.cohort, scales = "free_y") +
    labs(title=thisname, subtitle = paste0("Lines: Dashed = Outside Puget Sound, Dotted = Inside Puget Sound, Solid = Total population \n Light green = juvenile migrate to Ocean, Dark green = adult return, 
    Purple = Spawning, Magenta = Recruitment, \n
    Proportion juveniles migrating =", thismigiobox)) +
    scale_y_continuous(limits = c(0,NA)) +
    geom_vline(xintercept = thismigrationjuv$StartTofY,  
               color = "seagreen2", size=0.7) +
    geom_vline(xintercept = (thismigrationjuv$StartTofY + thismigrationjuv$Leave_Period),  
               color = "seagreen2",  size=0.7) +
    geom_vline(xintercept = thismigrationjuv$EndTofY,  
               color = "springgreen4",  size=0.7, alpha = 0.5) +
    geom_vline(xintercept = (thismigrationjuv$EndTofY + thismigrationad$Return_Period),  
               color = "springgreen4",  size=0.7, alpha = 0.5) +
    geom_vline(xintercept = thislifehistory$time_spawn,  
               color = "purple1",  linetype = "dotdash", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = (thislifehistory$time_spawn + thislifehistory$spawn_period),  
               color = "purple1", linetype = "dotdash", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = thislifehistory$recruit_time,  
               color = "magenta1", linetype = "dotdash", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = (thislifehistory$recruit_time + thislifehistory$recruit_period),  
               color = "magenta1", linetype = "dotdash", size=0.7, alpha = 0.5) +
    labs(x = "Day of year", y = "Numbers", colour = "Age class") +
    theme(plot.title = element_text(face="bold"))

  
  sp.nums.plot.out <- marray.nums %>% 
    ggplot() +
    geom_line(aes(x= time, y = DEN, colour = age))+
    scale_color_manual(values=col.pal) +
    facet_wrap(year ~ age, scales = "free_y") +
    labs(title=thisname, subtitle = paste0("Lines: Dashed = Outside Puget Sound, Dotted = Inside Puget Sound, Solid = Total population \n Light green = juvenile migrate to Ocean, Dark green = adult return, 
    Purple = Spawning, Magenta = Recruitment, \n
    Proportion juveniles migrating =", thismigiobox)) +
    scale_y_continuous(limits = c(0,NA)) +
    geom_vline(xintercept = thismigrationjuv$StartTofY,  
               color = "seagreen2", size=0.7) +
    geom_vline(xintercept = (thismigrationjuv$StartTofY + thismigrationjuv$Leave_Period),  
               color = "seagreen2",  size=0.7) +
    geom_vline(xintercept = thismigrationjuv$EndTofY,  
               color = "springgreen4",  size=0.7, alpha = 0.5) +
    geom_vline(xintercept = (thismigrationjuv$EndTofY + thismigrationad$Return_Period),  
               color = "springgreen4",  size=0.7, alpha = 0.5) +
    geom_vline(xintercept = thislifehistory$time_spawn,  
               color = "purple1",  linetype = "dotdash", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = (thislifehistory$time_spawn + thislifehistory$spawn_period),  
               color = "purple1", linetype = "dotdash", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = thislifehistory$recruit_time,  
               color = "magenta1", linetype = "dotdash", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = (thislifehistory$recruit_time + thislifehistory$recruit_period),  
               color = "magenta1", linetype = "dotdash", size=0.7, alpha = 0.5) +
    labs(x = "Day of year", y = "Numbers", colour = "Age class") +
    theme(plot.title = element_text(face="bold"))
  
    
  if(code.cohort>6){
  
      ggsave(here("figures",paste0(thiscode,"_daily_numbers.png")), sp.nums.plot.in, dpi= 300, width = 12, height = 10)
    
     ggsave(here("figures",paste0(thiscode,"_daily_numbers.pdf")), sp.nums.plot.in, dpi= 300, width = 12, height = 10)
    
  } else {

    ggsave(here("figures", paste0(thiscode,"_daily_numbers.png")), sp.nums.plot.in, dpi= 300, width = 14, height = 10)
    
    ggsave(here("figures", paste0(thiscode,"_daily_numbers.pdf")), sp.nums.plot.in, dpi= 300, width = 14, height = 10)
    
  }
  
  }
}

lapply(num.sp, plot_migration, migration.array, nums.csv, species.code, migration.lifehistory, migration.table, sim.years, this.folder)