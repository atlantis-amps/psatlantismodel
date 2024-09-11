# install packages
install.packages('devtools')   ## you need to do this step just once
# running
library("devtools")
install_github('Atlantis-Ecosystem-Model/ReactiveAtlantis', force=TRUE, dependencies=TRUE)
library("ReactiveAtlantis")
library(here)

nc.current  <- here::here("Atlantis_mv_1_6681_5yr_fprintfsp_rectype4_out1dy", "outputFolder","AMPS_OUT.nc")
nc.old      <- here::here("Atlantis_mv_1_6681M_5yr_fprintfsp_rectype4_out1dy", "outputFolder","AMPS_OUT.nc")
grp.csv     <- here::here("PugetSoundAtlantisFunctionalGroups.csv")
bgm.file    <- here::here("Atlantis_mv_1_6681M_5yr_fprintfsp_rectype4_out1dy","PugetSound_89b_070116.bgm")
cum.depths  <- c(0, 5, 25, 50, 150, 200, 350) ## This should be the cummulative depth of your mode ,Atlantis
## individual file
#compare(nc.current, nc.out.old = NULL, grp.csv, bgm.file, cum.depths)
## compare to previuos run
ReactiveAtlantis::compare(nc.current, nc.old, grp.csv, bgm.file, cum.depths)
