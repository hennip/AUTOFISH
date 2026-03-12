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

# Divide area to 4 pieces
# 1: NW from ruhnu 
# 2: NE from ruhnu 
# 3: SW from ruhnu 
# 4: SE from ruhnu 

source("data/trawl-hauls.R")

###############
# Catch data
###############

dfB_catch<-catch_all %>% mutate(year=SurveyYear)
#View(dfB_catch)
  
dfB_catch<-dfB_catch  |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> 
  mutate(catch=round(catch,0)) |> 
  mutate(CatchNumberAtLength=as.numeric(CatchNumberAtLength)) |> 
  mutate(CatchLengthClass=as.numeric(CatchLengthClass)) |> 
  full_join(df_rec) |> 
  mutate(species=ifelse(CatchSpeciesCode==126417,1,2))  |> # 1: Herring, 2:other
  select(year,rec_ruhnu, everything())|> 
  #select(-Catch, -Header, -HaulGear, -CruiseLocalID)|> 
  select(-CatchDataType, -CatchSpeciesValidity)
dfB_catch
#View(dfB_catch)


# Cobs: total catch per trawl haul
#########################################
# group by rec & haul, calculate total catch
TotCatch<-dfB_catch |>
  group_by(year,rec_ruhnu,HaulNumber) |> 
  summarise(tot_catch=sum(catch))|> 
  select(year,rec_ruhnu, HaulNumber, tot_catch)
#View(TotCatch)
TotCatch

# Calculate maximum number of hauls per rectangle in the data
tmp<-TotCatch |> group_by(year, rec_ruhnu,HaulNumber) |> summarise(n=n()) |> 
  select(-HaulNumber) |> summarise(n=n()) |> select(n)
max_number_of_hauls<-max(tmp$n)


# Cobs[h,r,y]
# Build tot catch table in which rows are hauls and columns are rectangles 
C_obs<-array(NA, dim=c(max_number_of_hauls,4,Nyears))
for(y in 1:Nyears){
  #y<-1
  for(r in 1:4){
  #r<-1
      apu<-1
  for(i in 1:length(TotCatch$rec_ruhnu)){
    if(TotCatch$year[i]==(y+min_years-1) & TotCatch$rec_ruhnu[i]==r){
      C_obs[apu,r,y]<-TotCatch$tot_catch[i]
      apu<-apu+1
    }}}
  }
C_obs

# Hobsprop: Proportion of herring in each catch
###############################################
# group by rec, haul & species, calculate total catch
herring<-dfB_catch  |> 
  group_by(year,rec_ruhnu,HaulNumber, species) |> 
  summarise(tot_catch=sum(catch))|> 
  select(year,rec_ruhnu, HaulNumber, species, tot_catch) |> 
  filter(species==1) |>  # herring only
  mutate(herring_catch=tot_catch) |> 
  select(-tot_catch)
herring
  
dfH<-full_join(herring, TotCatch) |> 
  mutate(hprop=herring_catch/tot_catch)


#HobsProp[h,r,y]
# Build table for herring proportions in which rows are hauls and columns are rectangles
Hprops<-array(NA, dim=c(max_number_of_hauls,4,Nyears))
for(y in 1:Nyears){
  for(r in 1:4){
  apu<-1
  for(i in 1:length(dfH$rec_ruhnu)){
    if(dfH$year[i]==(y+min_years-1) & dfH$rec_ruhnu[i]==r){
      Hprops[apu,r,y]<-dfH$hprop[i]
      apu<-apu+1
    }}}}
Hprops


# nLobs[r,s,y]: Total sample size per rectangle and species
# Lobs[1:8,r,s,y]: Number of fish of species s in all haul samples at rectangle r from length groups 1:8
#######################################################################################
# nLobs
#==========================
# catch sample size per ruhnu rectangle and species (1=herring, 2=other)
sample_size<-dfB_catch  |>
  group_by(year,rec_ruhnu,species) |> 
  summarise(tot_sample=sum(CatchNumberAtLength))#|> 
sample_size

# Sample size per species and rec in a form that feeds to the model
nL_obs<-array(NA, dim=c(4,2,Nyears))
for(y in 1:Nyears){
nL_obs[,1,y]<-as.data.frame(sample_size |> filter(year==(y+min_years-1), species==1) |>  
            pivot_wider(values_from = tot_sample, names_from = species))[,3]
nL_obs[,2,y]<-as.data.frame(sample_size |> filter(year==(y+min_years-1), species==2) |>  
            pivot_wider(values_from = tot_sample, names_from = species))[,3]
}
nL_obs

# Lobs
#==========================
# min and max length per species
dfB_catch |>group_by(year,CatchSpeciesCode) |>  
  summarise(min=min(CatchLengthClass), max=max(CatchLengthClass))

# Decide upon length groups
# limits are upper limits expect the last one which is also a lower limit of the 
# last group
length_limits_herring<-c(90,105,120,135,150,165,180) # 8 groups for herring
length_limits_other<-c(60,80,100,120,140) # 6 groups for other species

# Number of herring/other species in the sample per rectangle and length group
numbers_at_length_herring<-dfB_catch|>
  filter(species==1) |> 
  group_by(year,rec_ruhnu, CatchLengthClass)|>  
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
  group_by(year, rec_ruhnu, length_group) |> 
  summarise(number_at_length=sum(n)) |> 
  pivot_wider(names_from = rec_ruhnu, values_from=number_at_length) |> 
  arrange(year,length_group)
print(n=50, x=numbers_at_length_herring)


numbers_at_length_other<-dfB_catch|>
  filter(species==2) |> 
  group_by(year,rec_ruhnu, CatchLengthClass)|>  
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
  group_by(year, rec_ruhnu, length_group) |> 
  summarise(number_at_length=sum(n)) |> 
  pivot_wider(names_from = rec_ruhnu, values_from=number_at_length) |> 
  arrange(year,length_group)
print(n=50, x=numbers_at_length_other)




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
L_obs<-array(NA, dim=c(8,4,2,Nyears))
for(y in 1:Nyears){
  for(r in 1:4){
  L_obs[,r,1,y]<-as.data.frame(numbers_at_length |> filter(species==1 & year==(y+min_years-1)) |> 
                               ungroup() |> select(-year, -species, -length_group))[,r]
  L_obs[,r,2,y]<-as.data.frame(numbers_at_length |> filter(species==2 & year==(y+min_years-1)) |> 
                               ungroup() |> select(-year, -species, -length_group))[,r]
}}
L_obs


# NOTE! REPLACE NA's IN LENGTH DATA WHERE NA NOT SUITABLE OR IN REALITY 0
##########################################################################
# For computational reasons, sample size can't be missing so imput sample of 500 
# for all that are currently NA. Numbers per length will be then predicted by the model 
# (THIS PROPABLY DOES NOT HAPPEN BUT IF IT WOULD, THIS PIECE OF CODE WOULD DEAL WITH IT)
# AND
# In cases where sample was not missing, the NA's in G_obs should be replaced with 0s
for(r in 1:4){
  for(s in 1:2){
    for(y in 1:Nyears){
      if(is.na(nL_obs[r,s,y])==T){
        nL_obs[r,s,y]<-500}else{ # Input imaginary 500 sample where no sample was taken
          for(l in 1:8){
            if(is.na(L_obs[l,r,s,y])==T){
              L_obs[l,r,s,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
            }
          }
        }
    }
  }
}
L_obs
nL_obs

###############
# Herring Age data
###############

dfB_biol<-bio_all %>% mutate(year=SurveyYear) %>% mutate(HaulNumber=as.numeric(HaulNumber))

#dfB_biol$HaulNumber

# Add rectangles and define 8 length groups for herring
df_length_at_age<-dfB_biol|> 
  left_join(df_rec) |> # Add ruhnu rectangles based on year and haul number
  filter(CatchSpeciesCode==126417) |> 
  mutate(length=as.numeric(BiologyLengthClass), # shorten names
        age=as.numeric(BiologyIndividualAge))|> 
  mutate(length_group=ifelse(length<90, 1, NA)) |> 
  mutate(length_group=ifelse(length>=90  & length<105, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=105 & length<120, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=120 & length<135, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=135 & length<150, 5, length_group)) |> 
  mutate(length_group=ifelse(length>=150 & length<165, 6, length_group)) |> 
  mutate(length_group=ifelse(length>=165 & length<180, 7, length_group)) |> 
  mutate(length_group=ifelse(length>=180, 8, length_group))
df_length_at_age
#View(df_length_at_age |> select(length, length_group))

# Check for missing rectangle info, should be empty
df_length_at_age |> filter(is.na(rec_ruhnu)==T)

# Age without grouping by length, nice to know
df_pivot<-df_length_at_age |> 
  select(length, age) |> group_by(length, age) |> 
  summarise(n=n())|> # count works as each row is an individual 
  pivot_wider(names_from = age, values_from = n) |> 
  select(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`, `8`,`9`,`10`,`11`,`12`,`13`,`14`,`NA`)
print(n=35, x=df_pivot)
sum(df_pivot[,2:17], na.rm=T)# 9347
#View(df_pivot)


# Pool older ages to age group 9, remove missing ages
# Length as the first grouping argument keeps the length groups in correct order in the pivot table
df<-df_length_at_age |>
  mutate(age=ifelse(age>9, 9, age)) |>   # pool ages >=9 together (10th age group)
  filter(is.na(age)==F) |> # Remove individuals with missing age 
  group_by(length_group, year, rec_ruhnu, age) |> 
  summarise(n=n())
df
#View(df)
# This should be empty
df |> filter(is.na(age)==T)

# Pivot, not currently used
df_pivot<-  df|> 
  pivot_wider(names_from = length_group, values_from = n) 
View(df_pivot)

print(n=350, x=df_pivot) # Looks correct
sum(df_pivot[,4:11], na.rm=T) #9336


# nGobs[l,r,y]: Age sample size per length, rectangle and year
# Gobs[1:Nages,l,r,y]: Sample size on age samples from length group l
# ===================================================================
df
# Let's take ages 0-9 (10 age groups)
G_obs<-array(NA, dim=c(10,8,4,Nyears))
for(i in 1:dim(df)[1]){
  y<-df$year[i]-(min_years-1)
  r<-df$rec_ruhnu[i]
  r<-df$rec_ruhnu[i]
  l<-df$length_group[i]
  a<-df$age[i]+1 # first age groups is 0+ 
  G_obs[a,l,r,y]<-df$n[i]
}
G_obs
sum(G_obs, na.rm=T) # 9331

nG_obs<-array(NA, dim=c(8,4,Nyears))
for(y in 1:Nyears){
for(r in 1:4){
nG_obs[,r,y]<-as.data.frame(  df |> 
    filter(year==(y+min_years-1))  |> 
  summarise(ntot=sum(n))|> 
  pivot_wider(names_from = rec_ruhnu, values_from = ntot) |>
    select(length_group, year, `1`,`2`,`3`,`4`)|> # Order as pivot_wider may otherwise mess these up
    ungroup() |> 
    select(-length_group, -year))[,r] 
}
}
nG_obs
  
#nG_obs<-as.matrix(nG_obs)
sum(nG_obs, na.rm=T) # 2775 in 2022-2024

# NOTE! REPLACE NA's IN AGE DATA WHERE NA NOT SUITABLE OR ACTUALLY 0
#####################################################################
# For computational reasons, sample size can't be missing so imput sample of 500 
# for all that are currently NA. Number per age will be predicted by the model
# AND
# In cases where sample was not missing, the NA's in G_obs should be replaced with 0s
for(i in 1:8){
  for(r in 1:4){
    for(y in 1:Nyears){
      if(is.na(nG_obs[i,r,y])==T){
      nG_obs[i,r,y]<-500}else{ # Input imaginary 500 sample where no sample was taken
        for(a in 1:10){
          if(is.na(G_obs[a,i,r,y])==T){
            G_obs[a,i,r,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
          }
        }
      }
    }
  }
}
G_obs
nG_obs

G_obs[,,3,6]




