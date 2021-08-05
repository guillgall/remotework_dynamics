##4 steps

##1) install packages ggplot2, readxl, xtable, stargazer

library(ggplot2)
library(readxl)
library(xtable)
library(stargazer)
library(reshape2)
library(ggrepel)

##2) set your working directory (where the repository is located)

setwd("~/Documents/GitHub/remotework_dynamics")

##3) run the following code (this will run all other R codes)

rm(list=ls(all=TRUE))

source("./Code/01_merge_eis.R")
source("./Code/02_merge_eis_telework.R")
source("./Code/03_estimation_telework.R")
source("./Code/04_noc_crosswalk.R")
source("./Code/05_noc_remote_work.R")
source("./Code/06_lfs_merge.R")
source("./Code/07_lfs_remote_work.R")
source("./Code/08_lfs_employment_variation.R")
source("./Code/09_essential_service_variables.R")
source("./Code/10_worker_heterogeneity.R")

##4) open "./Code/11_tables_EIS.R" and "./Code/12_tables_LFS.R" run them plot by plot.
