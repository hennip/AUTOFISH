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

dfB_haul24<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=2)[1:16,] |> mutate(year=2024)
dfB_haul23<-read.csv(str_c(path,"Biotic_2023-ZR055_2024-02-06T10.01.18.377.csv"), skip=2)[1:17,] |> mutate(year=2023)

dfB_haul<-full_join(dfB_haul24, dfB_haul23) |> as_tibble()

dfB_haul<-dfB_haul|> 
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
  select( year, rec_ruhne, latStart, longStart, minDepth, maxDepth, HaulNumber, everything()) |> 
  select(-Haul, -Header)|> 
  mutate(rec=as.factor(rec_ruhne))
dfB_haul

df_rec<-dfB_haul |> select(year,rec_ruhne, HaulNumber);View(df_rec)

minmax_depth<-dfB_haul |> arrange(rec) |> group_by(year,rec) |> 
  mutate(rec=as.numeric(rec)) |> 
  summarise(min_trawl_depth=min(minDepth), max_trawl_depth=max(maxDepth))#, mean=(min+max)/2)


# df<-dfB_haul |> select(rec, minDepth, maxDepth, HaulNumber) |> 
#   pivot_longer(cols = minDepth:maxDepth, names_to="minmax",values_to = "depth")
# 
# ggplot(data=df, aes(x=HaulNumber, y=depth, group=rec))+
#   geom_line(aes(col=rec))

#View(dfB_haul)

###############
# Catch data
###############

dfB_catch24<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=19)[1:(500-20),]|> as_tibble() |> mutate(year=2024)
dfB_catch23<-read.csv(str_c(path,"Biotic_2023-ZR055_2024-02-06T10.01.18.377.csv"), skip=20)[1:(485-21),]|> as_tibble() |> mutate(year=2023) 

dfB_catch<-full_join(dfB_catch24,dfB_catch23) 
  #View(dfB_catch)
  
dfB_catch<-dfB_catch  |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> 
  mutate(catch=round(catch,0)) |> 
  mutate(CatchNumberAtLength=as.numeric(CatchNumberAtLength)) |> 
  mutate(CatchLengthClass=as.numeric(CatchLengthClass)) |> 
  full_join(df_rec) |> 
  mutate(species=ifelse(CatchSpeciesCode==126417,1,2))  |> # 1: Herring, 2:other
  select(year,rec_ruhne, everything())|> 
  select(-Catch, -Header, -HaulGear, -CruiseLocalID)|> 
  select(-CatchDataType, -CatchSpeciesValidity)
dfB_catch
#View(dfB_catch)


# Cobs: total catch per trawl haul
#########################################
# group by rec & haul, calculate total catch
TotCatch<-dfB_catch |>
  group_by(year,rec_ruhne,HaulNumber) |> 
  summarise(tot_catch=sum(catch))|> 
  select(year,rec_ruhne, HaulNumber, tot_catch)
#View(TotCatch)
TotCatch

# Cobs[h,r,y]
# Build tot catch table in which rows are hauls and columns are rectangles 
C_obs<-array(NA, dim=c(6,4,2))
for(y in 1:2){
for(r in 1:4){
  apu<-1
  for(i in 1:length(TotCatch$rec_ruhne)){
    if(TotCatch$year[i]==(y+2022) & TotCatch$rec_ruhne[i]==r){
      C_obs[apu,r,y]<-TotCatch$tot_catch[i]
      apu<-apu+1
    }}}}
C_obs

# Hobsprop: Proportion of herring in each catch
###############################################
# group by rec, haul & species, calculate total catch
herring<-dfB_catch  |> 
  group_by(year,rec_ruhne,HaulNumber, species) |> 
  summarise(tot_catch=sum(catch))|> 
  select(year,rec_ruhne, HaulNumber, species, tot_catch) |> 
  filter(species==1) |>  # herring only
  mutate(herring_catch=tot_catch) |> 
  select(-tot_catch)
herring
  
dfH<-full_join(herring, TotCatch) |> 
  mutate(hprop=herring_catch/tot_catch)


#HobsProp[h,r,y]
# Build table for herring proportions in which rows are hauls and columns are rectangles
Hprops<-array(NA, dim=c(6,4,2))
for(y in 1:2){
  for(r in 1:4){
  apu<-1
  for(i in 1:length(dfH$rec_ruhne)){
    if(dfH$year[i]==(y+2022) & dfH$rec_ruhne[i]==r){
      Hprops[apu,r,y]<-dfH$hprop[i]
      apu<-apu+1
    }}}}
Hprops


# nLobs[r,s,y]: Total sample size per rectangle and species
# Lobs[1:8,r,s,y]: Number of fish of species s in all haul samples at rectangle r from length groups 1:8
#######################################################################################
# nLobs
#==========================
# catch sample size per ruhne rectangle and species (1=herring, 2=other)
sample_size<-dfB_catch  |>
  group_by(year,rec_ruhne,species) |> 
  summarise(tot_sample=sum(CatchNumberAtLength))#|> 
sample_size

# Sample size per species and rec in a form that feeds to the model
nL_obs<-array(NA, dim=c(4,2,2))
for(y in 1:2){
nL_obs[,y,1]<-as.data.frame(sample_size |> filter(year==(y+2022), species==1) |>  
            pivot_wider(values_from = tot_sample, names_from = species))[,3]
nL_obs[,y,2]<-as.data.frame(sample_size |> filter(year==(y+2022), species==2) |>  
            pivot_wider(values_from = tot_sample, names_from = species))[,3]
}
nL_obs

# Lobs
#==========================
# min and max length per species
dfB_catch |>group_by(year,CatchSpeciesCode) |>  
  summarise(min=min(CatchLengthClass), max=max(CatchLengthClass))

# Decide upon 8 length groups
# limits are upper limits expect the last one which is also a lower limit of the 
# 8th group
length_limits<-c(90,105,120,135,150,165,180)

# Number of herring/other species in the sample per rectangle and length group
numbers_at_length<-dfB_catch|>
  group_by(year,rec_ruhne, species, CatchLengthClass)|>  
  summarise(n=sum(CatchNumberAtLength)) |> 
  mutate(LengthClass=CatchLengthClass) |> 
  mutate(length_group=ifelse(LengthClass<90, 1, NA)) |> 
  mutate(length_group=ifelse(LengthClass>=90  & LengthClass<105, 2, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=105 & LengthClass<120, 3, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=120 & LengthClass<135, 4, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=135 & LengthClass<150, 5, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=150 & LengthClass<165, 6, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=165 & LengthClass<180, 7, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=180, 8, length_group)) |> 
  group_by(year,rec_ruhne, species, length_group) |> 
  summarise(number_at_length=sum(n)) |> 
  pivot_wider(names_from = rec_ruhne, values_from=number_at_length) |> 
  arrange(year,species, length_group)
  
print(n=50, x=numbers_at_length)

# mean lengths in 8 length groups
meanL<-c()
# <90
meanL<-as.data.frame(dfB_catch|>
  mutate(length=CatchLengthClass) |> 
  filter(length<90, species==1) |> select(-species) |> 
  summarise(meanL=mean(length)))[[1]]
            
for(i in 1:6){
  #i<-1
  meanL[i+1]<-length_limits[i]+(length_limits[i+1]-length_limits[i])/2
}

# >180
meanL[8]<-
  as.data.frame(
  dfB_catch|>
    mutate(length=CatchLengthClass) |> 
    filter(length>180, species==1) |> select(-species) |> 
    summarise(meanL=mean(length))
  )[[1]]

meanL



# Sample size per species and rec in a form that feeds to the model
# Lobs[1:8,r,s,y]
L_obs<-array(NA, dim=c(8,4,2,2))
for(y in 1:2){
  for(r in 1:4){
  L_obs[,r,1,y]<-as.data.frame(numbers_at_length |> filter(species==1 & year==(y+2022)) |> 
                               ungroup() |> select(-year, -species, -length_group))[,r]
  L_obs[,r,2,y]<-as.data.frame(numbers_at_length |> filter(species==2 & year==(y+2022)) |> 
                               ungroup() |> select(-year, -species, -length_group))[,r]
}}
L_obs


###############
# Herring Age data
###############

# Read in annual data
dfB_biol24<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=500) |> mutate(year=2024)|> as_tibble()
dfB_biol23<-read.csv(str_c(path,"Biotic_2023-ZR055_2024-02-06T10.01.18.377.csv"), skip=485) |> mutate(year=2023)|> as_tibble()
dfB_biol22<-read.csv(str_c(path,"Biotic_2022-ZR033_2023-04-13T07.55.56.180.csv"), skip=509) |> mutate(year=2022)|> as_tibble()
dfB_biol21<-read.csv(str_c(path,"Biotic_2021-ZR007_2022-03-11T16.00.12.580.csv"), skip=488) |> mutate(year=2021)|> as_tibble()
dfB_biol20<-read.csv(str_c(path,"Biotic_2020-ZR005_2021-04-01T13.30.21.857.csv"), skip=587) |> mutate(year=2020)|> as_tibble()

# Join
dfB_biol_all<-full_join(dfB_biol24, dfB_biol23) #|> 
  #full_join(dfB_biol22) |> 
  #full_join(dfB_biol21) |> 
  #full_join(dfB_biol20)

dfB_biol<-dfB_biol_all

# Add rectangles and define 8 length groups for herring
df_length_at_age<-dfB_biol|> 
  left_join(df_rec) |> # Add ruhne rectangles based on year and haul number
  filter(CatchSpeciesCode==126417) |> 
  mutate(length=BiologyLengthClass, # shorten names
        age=BiologyIndividualAge)|> 
  mutate(length_group=ifelse(length<90, 1, NA)) |> 
  mutate(length_group=ifelse(length>=90  & length<105, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=105 & length<120, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=120 & length<135, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=135 & length<150, 5, length_group)) |> 
  mutate(length_group=ifelse(length>=150 & length<165, 6, length_group)) |> 
  mutate(length_group=ifelse(length>=165 & length<180, 7, length_group)) |> 
  mutate(length_group=ifelse(length>=180, 8, length_group))
df_length_at_age
#View(df_length_at_age)

# Check for missing rectangle info, should be empty
df_length_at_age |> filter(is.na(rec_ruhne)==T)

# Age without grouping, nice to know
df_pivot<-df_length_at_age |> 
  select(length, age) |> group_by(length, age) |> 
  summarise(n=n())|> # count works as each row is an individual 
  pivot_wider(names_from = age, values_from = n) |> 
  select(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`, `8`,`9`,`10`,`11`,`12`,`13`,`14`,`NA`)
print(n=35, x=df_pivot)
sum(df_pivot[,2:17], na.rm=T)# 2786 in 2023-2024 #7803 in 2020-2024

# Pool older ages to age group 9, remove missing ages
# Length as the first grouping argument keeps the length groups in correct order in the pivot table
df<-df_length_at_age |>
  mutate(age=ifelse(age>9, 9, age)) |>   # pool ages >=9 together (10th age group)
  filter(is.na(age)==F) |> # Remove individuals with missing age 
  group_by(length_group, year, rec_ruhne, age) |> 
  summarise(n=n())
df

# should be empty
df |> filter(is.na(age)==T)

# Pivot, not currently used
df_pivot<-  df|> 
  pivot_wider(names_from = length_group, values_from = n) 
print(n=350, x=df_pivot) # Looks correct
sum(df_pivot[,4:11], na.rm=T) #2775


# nGobs[l,r,y]: Age sample size per length, rectangle and year
# Gobs[1:Nages,l,r,y]: Sample size on age samples from length group l
# ===================================================================
df
# Let's take ages 0-9 (10 age groups)
G_obs<-array(NA, dim=c(10,8,4,2))
for(i in 1:dim(df)[1]){
  y<-df$year[i]-2022
  r<-df$rec_ruhne[i]
  r<-df$rec_ruhne[i]
  l<-df$length_group[i]
  a<-df$age[i]+1 # first age groups is 0+ 
  G_obs[a,l,r,y]<-df$n[i]
}
G_obs
sum(G_obs, na.rm=T) # 1210!

nG_obs<-array(NA, dim=c(8,4,2))

for (y in 1:2){
for(r in 1:4){
nG_obs[,r,y]<-as.data.frame(  df |> 
    filter(year==(y+2022))  |> 
  summarise(ntot=sum(n))|> 
  pivot_wider(names_from = rec_ruhne, values_from = ntot) |>
    ungroup() |> 
    select(-length_group, -year))[,r] 
}

}
  
nG_obs
sum(nG_obs, na.rm=T) # 2775 in 2022-2024

