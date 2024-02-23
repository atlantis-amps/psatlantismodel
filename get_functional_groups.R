#Get new functional group file

library(googlesheets4)
library(here)

gs4_deauth()

func.groups <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-8ETIElzhLNl3FTZ4aglciKd2CERgYQCfXVZky_Zg3E/edit?usp=sharing", sheet="aurelia_branch")

readr::write_csv(func.groups, here::here("atlantis_mv_1_6665","PugetSoundAtlantisFunctionalGroups.csv"))