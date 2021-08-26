##################-
### Author: Peter Kress
### Date:  Wed Aug 25 22:34:12 2021
### Purpose: Purpose
##################-


##################-
# Initialize Workspace ----
##################-

setwd("~/Documents/Personal Projects/Music Reviews/")

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , googlesheets4)

`%p%` = paste0

##################-
# Pull Concert Data ----
##################-


### Last Edited:  Wed Aug 25 22:34:12 2021