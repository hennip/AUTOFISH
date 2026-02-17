source("packages-and-paths.r")

####################################
# Read in the trawl data from GRAHS (Gulf of Riga acoustic survey)
# and modify it for the needs of model run
# Note that the Biotic-data file contains three data frames:
# 1. Haul information
# 2. Catch data
# 3. Biological data, incl. length and age
####################################

# Instead of statistical rectangles, divide the gulf into 4 areas using coordinates
# of Ruhne island (lighthouse) as a limit point

# Coordinates of Ruhne lighthouse according to Wikipedia
ruhneLat<-57.80135766
ruhneLong<-23.26012233

# Divide area to 4 pieces
# 1: NW from Ruhne 
# 2: NE from Ruhne 
# 3: SW from Ruhne 
# 4: SE from Ruhne 

###############
# Haul data
###############

dfB_haul<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=2)[1:16,] |> 
  as_tibble()|> 
  mutate(#HaulStartTime=as.Date(HaulStartTime),
    HaulNumber=as.numeric(HaulNumber),
    latStart=as.numeric(HaulStartLatitude),
    longStart=as.numeric(HaulStartLongitude),
    latStop=as.numeric(HaulStopLatitude),
    longStop=as.numeric(HaulStopLongitude)) |> 
  mutate(rec_ruhne=ifelse(latStart>=ruhneLat & longStart<ruhneLong, 1,NA)) |>         # 1: NW
  mutate(rec_ruhne=ifelse(latStart>=ruhneLat & longStart>=ruhneLong, 2,rec_ruhne)) |> # 2: NE 
  mutate(rec_ruhne=ifelse(latStart<ruhneLat & longStart<ruhneLong, 3,rec_ruhne)) |>   # 3: SW 
  mutate(rec_ruhne=ifelse(latStart<ruhneLat & longStart>=ruhneLong, 4,rec_ruhne)) |>  # 4: SE 
  mutate(minDepth=as.numeric(HaulMinTrawlDepth), maxDepth=as.numeric(HaulMaxTrawlDepth)) |> 
  select( rec_ruhne, latStart, longStart, minDepth, maxDepth, HaulNumber, everything()) |> select(-Haul, -Header)|> 
  mutate(rec=as.factor(rec_ruhne))
dfB_haul

df_rec<-dfB_haul |> select(rec_ruhne, HaulNumber)


minmax_depth<-dfB_haul |> arrange(rec) |> group_by(rec) |> 
  mutate(rec=as.numeric(rec)) |> 
  summarise(min_trawl_depth=min(minDepth), max_trawl_depth=max(maxDepth))#, mean=(min+max)/2)


# df<-dfB_haul |> select(rec, minDepth, maxDepth, HaulNumber) |> 
#   pivot_longer(cols = minDepth:maxDepth, names_to="minmax",values_to = "depth")
# 
# ggplot(data=df, aes(x=HaulNumber, y=depth, group=rec))+
#   geom_line(aes(col=rec))

#View(dfB_haul)

###############
# Haul data
###############


dfB_catch<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=19)[1:(500-20),] |> 
  as_tibble() |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> 
  mutate(catch=round(catch,0)) |> 
  full_join(df_rec) |> 
  mutate(species=ifelse(CatchSpeciesCode==126417,1,2))  |> # 1: Herring, 2:other
  select(rec_ruhne, everything())|> 
  select(-Catch, -Header, -HaulGear, -CruiseLocalID)|> 
  select(-CatchDataType, -CatchSpeciesValidity)
dfB_catch
#View(dfB_catch)


# Cobs: total catch per trawl haul
#########################################
# group by rec & haul, calculate total catch
TotCatch<-dfB_catch |>
  group_by(rec_ruhne,HaulNumber) |> 
  summarise(tot_catch=sum(catch))|> 
  select(rec_ruhne, HaulNumber, tot_catch)
#View(TotCatch)
TotCatch

# Cobs[h,r,y]
# Build tot catch table in which rows are hauls and columns are rectangles 
C_obs<-array(NA, dim=c(6,4))
for(r in 1:4){
  apu<-1
  for(i in 1:length(TotCatch$rec_ruhne)){
    if(TotCatch$rec_ruhne[i]==r){
      C_obs[apu,r]<-TotCatch$tot_catch[i]
      apu<-apu+1
    }}}
C_obs

# Hobsprop: Proportion of herring in each catch
###############################################
# group by rec, haul & species, calculate total catch
herring<-dfB_catch  |> 
  group_by(rec_ruhne,HaulNumber, species) |> 
  summarise(tot_catch=sum(catch))|> 
  select(rec_ruhne, HaulNumber, species, tot_catch) |> 
  filter(species==1) |>  # herring only
  mutate(herring_catch=tot_catch) |> 
  select(-tot_catch)
herring
  
dfH<-full_join(herring, TotCatch) |> 
  mutate(hprop=herring_catch/tot_catch)


#HobsProp[h,r,y]
# Build table for herring proportions in which rows are hauls and columns are rectangles
Hprops<-array(NA, dim=c(6,4))
for(r in 1:4){
  apu<-1
  for(i in 1:length(dfH$rec_ruhne)){
    if(dfH$rec_ruhne[i]==r){
      Hprops[apu,r]<-dfH$hprop[i]
      apu<-apu+1
    }}}
Hprops


# Lobs: Number of fish of species s from all hauls at rectangle r from length groups 1:8
#######################################################################################
# Lobs[1:8,r,s,y]
# nLobs[r,s,y])







###############
# Age data
###############


dfB_biol<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=500) |> 
  as_tibble() #|> 
#  mutate(HaulNumber=as.numeric(HaulNumber))
dfB_biol




