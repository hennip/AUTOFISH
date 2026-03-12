source("packages-and-paths.R")

####################################
# Read in the trawl data from GRAHS (Gulf of Riga acoustic survey)
# and modify it for the needs of model run
# Note that the Biotic-data file contains three data frames:
# 1. Haul information
# 2. Catch data
# 3. Biological data, incl. length and age
####################################
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
source("data/trawl-catches.R")
source("data/trawl-biotic.R")


