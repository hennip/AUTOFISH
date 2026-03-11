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
})


#whoisthis<-"hp_laptop"
#whoisthis<-"hp_kala1"
#whoisthis<-"es_laptop"

if(whoisthis=="hp_laptop"){
  pathA<-"../../01-Projects/AUTOFISH/dat/orig/Acoustic/"
  pathB<-"../../01-Projects/AUTOFISH/dat/orig/Biotic/"
}
if(whoisthis=="hp_kala1"){path<-"../../dat/AUTOFISH/orig/"}
if(whoisthis=="es_laptop"){pathB<-path<-"../../ICES Acoustic database/GRAHS/"}


