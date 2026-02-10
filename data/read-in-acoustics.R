library(readxl)
library(tidyverse)
library(forcats)

path<-"../../01-Projects/AUTOFISH/dat/orig/"

dfA<-read.csv(str_c(path,"Acoustic_2024-ZR038_2025-03-12T10.25.26.053.csv"), skip=11) |> 
  as_tibble() 
dfA
View(dfA)


# Coordinates to Ruhne lighthouse according to Wikipedia
ruhneLat<-57.80135766
ruhneLong<-23.26012233

# Divide area to 4 pieces:
# 1: NW from Ruhne 
# 2: NE from Ruhne 
# 3: SW from Ruhne 
# 4: SE from Ruhne 

dfA<-dfA |> mutate(rec_ruhne=ifelse(LogLatitude>=ruhneLat & LogLongitude<ruhneLong, 1,NA)) |>      # 1: NW
  mutate(rec_ruhne=ifelse(LogLatitude>=ruhneLat & LogLongitude>=ruhneLong, 2,rec_ruhne)) |>  # 2: NE 
  mutate(rec_ruhne=ifelse(LogLatitude<ruhneLat & LogLongitude<ruhneLong, 3,rec_ruhne)) |>  # 3: SW 
  mutate(rec_ruhne=ifelse(LogLatitude<ruhneLat & LogLongitude>=ruhneLong, 4,rec_ruhne))     # 4: SE 
  
  
  dfA |> select(rectangle_ruhne, everything())

LogLongitude




dfB_haul<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=2)[1:16,] |> 
  as_tibble()|> 
  mutate(#HaulStartTime=as.Date(HaulStartTime),
         HaulNumber=as.numeric(HaulNumber),
         HaulStartLatitude=as.numeric(HaulStartLatitude),
         HaulStartLongitude=as.numeric(HaulStartLongitude),
         HaulStopLatitude=as.numeric(HaulStopLatitude),
         HaulStopLongitude=as.numeric(HaulStopLongitude))
dfB_haul


dfB_catch<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=19) |> 
  as_tibble()
dfB_catch





