source("packages-and-paths.R")

# =====================================================
# Read in the acoustic and trawl data from GRAHS (Gulf of Riga acoustic survey)
# and modify those for the needs of Bayes model
# Note that the Biotic-data file contains three data frames:
# 1. Haul information
# 2. Catch data
# 3. Biological data, incl. length and age
####################################

# If needed, define years to take into account
min_year<-2023
max_year<-2024

source("data/read-in-acoustic-data.R") 


source("data/read-in-trawl-data.R") 

# Instead of statistical rectangles, divide the gulf into 4 areas using coordinates
# of ruhnu island (lighthouse) as a limit point

# Coordinates of ruhnu lighthouse according to Wikipedia
ruhnuLat<-57.80135766
ruhnuLong<-23.26012233

# Numbers of ruhnu rectangles are
# 1: NW from ruhnu 
# 2: NE from ruhnu 
# 3: SW from ruhnu 
# 4: SE from ruhnu 

source("data/trawl-hauls.R")

# Define length groups for each species/species group
# limits are upper limits expect the last one which is also the lower limit 
# of the last group
# NOTE THAT THE UPPER AND LOWER LIMITS NEED TO BE DEFINED
# IN trawl-catches.R AS WELL!!!
length_limits_herring<-c(90,105,120,135,150,165,180) # 8 groups for herring
length_limits_other<-c(60,80,100,120,140) # 6 groups for other species
source("data/trawl-catches.R")


source("data/trawl-biotic.R")


