#4 steps

#1) install packages ggplot2, readxl, xtable, stargazer

#2) set your working directory (where the repository is located)

setwd("~/Documents/GitHub/remoteworkdynamics_canada")

#3) run the following code (this will run all other R codes)

rm(list=ls(all=TRUE))

source("./Code/R/1_merge_eis.R")

source("./Code/R/2_merge_eis_telework.R")

source("./Code/R/3_estimation_telework.R")

#4) open "4_tables.R" and run, as well as "4_tables_LFS.R" and run


