################################################################################
# R script for calculation of BIAS results either for single country data
# or multiple countries simultaneously
# Note that the country specific results may differ in single country vs all
# countries run since the age length key combines data on ICES SD basis  
#
# This script was made in 2026 in the Interreg project AUTOFISH
#
################################################################################

rm(list = ls())

# Input
###############################

# Define year
choose_year<-2025

# Define paths 
#pathA<- # Path for acoustic data
pathB<-pathA # Path for trawl data, if different than pathA
# path_output<- # Path on which you wish to have the output stored

# Define country or countries whose results you wish to calculate:
# ====================================================================

# Option 1: Choose this if you wish to run only one country data
# =================================
# Supported country abbreviations are "EE", "FI","SE","DE","PL", "LV" and "LT
#countries<-"EE"

# Option 2: Choose this if you wish to run multiple countries at once yet
# =================================
# separatedly from each other (i.e age length keys are not combined)
#countries<-c("EE", "FI","SE","DE","PL", "LV") # LT not yet supported as data is not available

# Option 3: Choose this if you wish to run the script by pooling data from all 
# =================================
# countries. The age length key's are formulated on ICES SD basis
countries<-"all"

Ncountries<-length(countries)

for(i in 1:Ncountries){
  country<-countries[i]    
  # Read in data 
  source("02-current-method/read-in-data-BIAS-all-countries.R")
  
  # Run the script for calculating BIAS results
  source("02-current-method/current_BIAS_calculation.R")
  
  # Save the results
  write_xlsx(res,paste0(path_output, "BIAS_results_", choose_year,"_", country,".xlsx"))
}
