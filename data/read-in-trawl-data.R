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
# Catch data
###############


dfB_catch<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=19)[1:(500-20),] |> 
  as_tibble() |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> 
  mutate(catch=round(catch,0)) |> 
  mutate(CatchNumberAtLength=as.numeric(CatchNumberAtLength)) |> 
  mutate(CatchLengthClass=as.numeric(CatchLengthClass)) |> 
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


# nLobs[r,s,y]: Total sample size per rectangle and species
# Lobs[1:8,r,s,y]: Number of fish of species s in all haul samples at rectangle r from length groups 1:8
#######################################################################################
# nLobs
#==========================
# catch sample size per ruhne rectangle and species (1=herring, 2=other)
sample_size<-dfB_catch  |>
  group_by(rec_ruhne,species) |> 
  summarise(tot_sample=sum(CatchNumberAtLength))#|> 
sample_size

# Sample size per species and rec in a form that feeds to the model
nL_obs<-array(NA, dim=c(4,2))
nL_obs<-as.data.frame(sample_size |> 
            pivot_wider(values_from = tot_sample, names_from = species))[,2:3]
nL_obs

# Lobs
#==========================
# min and max length per species
dfB_catch |>group_by(CatchSpeciesCode) |>  
  summarise(min=min(CatchLengthClass), max=max(CatchLengthClass))

# Decide upon 8 length groups
# limits are upper limits expect the last one which is also a lower limit of the 
# 8th group
length_limits<-c(90,105,120,135,150,165,180)

# Number of herring/other species in the sample per rectangle and length group
numbers_at_length<-dfB_catch|>
  group_by(rec_ruhne, species, CatchLengthClass)|>  
  summarise(n=sum(CatchNumberAtLength)) |> 
  mutate(LengthClass=CatchLengthClass)
  mutate(length_group=ifelse(LengthClass<90, 1, NA)) |> 
  mutate(length_group=ifelse(LengthClass>=90  & LengthClass<105, 2, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=105 & LengthClass<120, 3, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=120 & LengthClass<135, 4, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=135 & LengthClass<150, 5, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=150 & LengthClass<165, 6, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=165 & LengthClass<180, 7, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=180, 8, length_group)) |> 
  group_by(rec_ruhne, species, length_group) |> 
  summarise(number_at_length=sum(n)) |> 
  pivot_wider(names_from = rec_ruhne, values_from=number_at_length) |> 
  arrange(species, length_group)
  
print(n=50, x=numbers_at_length)

# Sample size per species and rec in a form that feeds to the model
L_obs<-array(NA, dim=c(8,4,2))
for(r in 1:4){
  L_obs[,r,1]<-as.data.frame(numbers_at_length |> filter(species==1) |> ungroup() |> select(-species, -length_group))[,r]
  L_obs[,r,2]<-as.data.frame(numbers_at_length |> filter(species==2) |> ungroup() |> select(-species, -length_group))[,r]
}
L_obs


###############
# Herring Age data
###############

dfB_biol24<-read.csv(str_c(path,"Biotic_2024-ZR038_2025-03-12T10.40.08.950.csv"), skip=500) |> mutate(year=2024)|> as_tibble()
dfB_biol23<-read.csv(str_c(path,"Biotic_2023-ZR055_2024-02-06T10.01.18.377.csv"), skip=485) |> mutate(year=2023)|> as_tibble()
dfB_biol22<-read.csv(str_c(path,"Biotic_2022-ZR033_2023-04-13T07.55.56.180.csv"), skip=509) |> mutate(year=2022)|> as_tibble()
dfB_biol21<-read.csv(str_c(path,"Biotic_2021-ZR007_2022-03-11T16.00.12.580.csv"), skip=488) |> mutate(year=2021)|> as_tibble()
dfB_biol20<-read.csv(str_c(path,"Biotic_2020-ZR005_2021-04-01T13.30.21.857.csv"), skip=587) |> mutate(year=2020)|> as_tibble()

dfB_biol_all<-full_join(dfB_biol24, dfB_biol23) |> 
  full_join(dfB_biol22) |> 
  full_join(dfB_biol21) |> 
  full_join(dfB_biol20)

dfB_biol<-dfB_biol_all

df_length_at_age<-dfB_biol|> 
  left_join(df_rec) |> # Add ruhne rectangles 
  filter(CatchSpeciesCode==126417) |> 
  mutate(length=BiologyLengthClass, # Give shorter names
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
df_length_at_age |> filter(is.na(rec_ruhne)==T)

# Obs! Data from all years
df_pivot<-df_length_at_age |> 
  select(length, age) |> group_by(length, age) |> 
  summarise(n=n())|> # count works as each row is an individual 
  pivot_wider(names_from = age, values_from = n) |> 
  select(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`, `8`,`9`,`10`,`11`,`12`,`13`,`14`,`NA`)
print(n=35, x=df_pivot)
sum(df_pivot, na.rm=T) #12178 2020-2024

# Divide to rectangles, but use only 2024 data as atm rec_ruhne contains only that year
df_pivot2<-df_length_at_age |> 
  filter(year==2024) |> 
  mutate(age=ifelse(age>9, 9, age)) |>  # pool ages >=9 together (10th age group)
  group_by(rec_ruhne, length_group, age) |> summarise(n=n())|> 
  pivot_wider(names_from = age, values_from = n) 
print(n=350, x=df_pivot2) # Looks correct


#|> 
  select(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`, `8`,`9`,`10`,`11`,`12`,`13`,`14`,`NA`)
sum(df_length_age_pivot, na.rm=T)

tmp<-df_length_at_age |> select(rec_ruhne, everything())
filter(tmp, is.na(rec_ruhne)==T)
View(tmp)

# nGobs[l,r,y]: Age sample size 
# Gobs[1:Nages,l,r,y]: Sample size on age samples from length group l
# ===================================================================


df_ages<-dfB_biol24 |> select(-Biology, -Header, -CruiseLocalID, -HaulGear) |> 
  filter(CatchSpeciesCode==126417) |> 
  mutate(LengthClass=BiologyLengthClass) |> 
  mutate(age=BiologyIndividualAge) |> 
  mutate(length_group=ifelse(LengthClass<90, 1, NA)) |> 
  mutate(length_group=ifelse(LengthClass>=90  & LengthClass<105, 2, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=105 & LengthClass<120, 3, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=120 & LengthClass<135, 4, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=135 & LengthClass<150, 5, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=150 & LengthClass<165, 6, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=165 & LengthClass<180, 7, length_group)) |> 
  mutate(length_group=ifelse(LengthClass>=180, 8, length_group)) |> 
  select(rec_ruhne, HaulNumber,length_group, age, year)
df_ages
#View(df_ages) # 1210 individuals in total 

tmp<-df_ages |> group_by(rec_ruhne, length_group, age) #|>  
  summarise(n=n())  
#tmp2<-pivot_wider(tmp, names_from = length_group, values_from = n)
#print(n=50, tmp2)

# Let's take ages 0-9 (10 age groups)
G_obs<-array(NA, dim=c(10,8,4))
for(i in 1:dim(tmp)[1]){
  r<-tmp$rec_ruhne[i]
  l<-tmp$length_group[i]
  a<-ifelse(tmp$age[i]<=9,tmp$age[i]+1,9) # age groups start from 1 (0+) 
  G_obs[a,l,r]<-tmp$n[i]
}
G_obs
sum(G_obs, na.rm=T)

filter(tmp, length_group==2)

# Tämä voi olla ok, ->nGobs
nG_obs<-as.data.frame(df |> group_by(rec_ruhne, length_group) |> summarise(n=n()) |> 
  pivot_wider(values_from = n, names_from = rec_ruhne) |> 
    ungroup() |> select(-length_group))
nG_obs

# Sitten tarvii Gobsin, jossa jokaiselle ruudulle oma age-length matriisi. Easy peasy?
