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

# Define data
###############################

# Select data from one country or choose all countries
# Currently other options (eg two countries) are not available

#all_countries<-"yes" # In this option you need to have all countries data available
all_countries<-"no" # Choose this if you want results from single country data only

if(all_countries=="no"){
  # Supported abbreviations are EE, FI, SE, DE, PL, LV, LT
  country<-"EE" 
}

# Define year
choose_year<-2025

# Read in data 
source("02-current-method/read-in-data-BIAS-all-countries.R")

# Run the script for calculating BIAS results
source("02-current-method/current_BIAS_calculation.R")

# Save the results
if(all_countries=="no"){
  write_xlsx(res,paste0(path_output, "BIAS_results_", choose_year,"_", country,".xlsx"))
}
if(all_countries=="yes"){
  write_xlsx(res,paste0(path_output, "BIAS_results_", choose_year, "_all_countries.xlsx"))
}
