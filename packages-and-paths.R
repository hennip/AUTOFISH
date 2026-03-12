# First define which computer is this. This helps defining the path.
# After running line whoisthis, comment it out again 
# to avoid unnecessary conflict solving in git.

#whoisthis<-"hp_laptop"
#whoisthis<-"hp_kala1"
#whoisthis<-"es_laptop"


suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(forcats)
  library(readr)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(runjags)
  library(coda)
})

if(whoisthis=="hp_laptop"){
  pathA<-"../../01-Projects/AUTOFISH/dat/orig/Acoustic/"
  pathB<-"../../01-Projects/AUTOFISH/dat/orig/Biotic/"
}
if(whoisthis=="hp_kala1"){path<-"../../dat/AUTOFISH/orig/"}
if(whoisthis=="es_laptop"){pathA<-pathB<-"../../ICES Acoustic database/GRAHS/"}


