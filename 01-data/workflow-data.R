
# =====================================================
# Read in the acoustic and trawl data from GRAHS (Gulf of Riga acoustic survey)
# and modify those for the needs of Bayes model
# Note that the Biotic-data file contains three data frames:
# 1. Haul information
# 2. Catch data
# 3. Biological data, incl. length and age
####################################

source("00-basics/packages-and-paths.R")

# Coordinates of ruhnu lighthouse according to Wikipedia. These´will be used
# to refine new rectangles
ruhnuLat<-57.80135766
ruhnuLong<-23.26012233

# Numbers of ruhnu rectangles are
# 1: NW from ruhnu 
# 2: NE from ruhnu 
# 3: SW from ruhnu 
# 4: SE from ruhnu 


# If needed, define years to take into account
min_year<-2023
max_year<-2024

source("01-data/read-in-acoustic-data.R") 
source("01-data/modify-acoustic-data.R") 

source("01-data/read-in-trawl-data.R") 

source("01-data/trawl-hauls.R")

# Define length groups for each species/species group
# limits are upper limits expect the last one which is also the lower limit 
# of the last group
# NOTE THAT THE UPPER AND LOWER LIMITS NEED TO BE DEFINED
# IN trawl-catches.R AS WELL!!!
length_limits_herring<-c(90,105,120,135,150,165,180) # 8 groups for herring
length_limits_other<-c(60,80,100,120,140) # 6 groups for other species
source("01-data/trawl-catches.R")

source("01-data/trawl-biotic.R")


