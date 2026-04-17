
################################################################################
# IMPORTANT!!!
################################################################################
# First define your computer name. This enables saving personal path definitions
# without needing to change the code each time. This also helps in avoidin
# unnecessary conflict solving in git.

# DURING FIRST USAGE OF THE SCRIPTS ON NEW COMPUTER, DO THIS:
#=============================================================
# 1. CREATE A NEW SCRIPT IN WHICH YOU DEFINE YOUR COMPUTER NAME 
# eg. (but remove #)
#computer_name<-"hp_laptop"
# 2. SAVE THIS NEW SCRIPT ONE FOLDER ABOVE YOUR REPO FOLDER WITH NAME
# computer_name.R
# 3. DEFINE YOUR COMPUTER SPECIFIC SETTINGS (pathA, pathB etc.) BELOW (IN THIS FILE) AND SAVE
# 4. YOU CAN NOW RUN packages-and-paths.R (THIS FILE) AND THE REST OF FILES 
# SHOULD FOLLOW YOUR CORRECT SETTINGS


source("../computer_name.R")

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(tidyverse)
  library(forcats)
  library(readr)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(rjags)
  library(runjags)
  library(coda)
  library(mapplots)
  
})

if(computer_name=="hp_laptop"){
  
  path_BIAS<-"../../01-Projects/AUTOFISH/dat/BIAS_24/"
  
  pathA<-"../../01-Projects/AUTOFISH/dat/orig/Acoustic/"
  pathB<-"../../01-Projects/AUTOFISH/dat/orig/Biotic/"
}
if(computer_name=="hp_kala1"){pathA<-pathB<-"../../dat/AUTOFISH/orig/"}
if(computer_name=="es_laptop"){pathA<-pathB<-"../../ICES Acoustic database/GRAHS/"}


