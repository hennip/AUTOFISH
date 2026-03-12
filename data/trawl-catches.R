###############
# Catch data
###############

dfB_catch<-catch_all %>% mutate(year=SurveyYear)|> 
  filter(year>=min_year & year<=max_year)
#View(dfB_catch)

dfB_catch<-dfB_catch  |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> 
  mutate(catch=round(catch,0)) |> 
  mutate(CatchNumberAtLength=as.numeric(CatchNumberAtLength)) |> 
  mutate(length=as.numeric(CatchLengthClass)) |> 
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
  summarise(min=min(length), max=max(length))

# Define length groups per species and calculate
# number of individuals in each group
# NOTE! Number of groups differs for different species

numbers_at_length<-dfB_catch  |> 
  group_by(year,rec_ruhnu, species, length)|>  
summarise(n=sum(CatchNumberAtLength)) 
numbers_at_length

# Number of herring/other species in the sample per rectangle and length group
numbers_at_length_herring<-numbers_at_length|>
  filter(species==1) |> 
  mutate(length_group=ifelse(length<90, 1, NA)) |> 
  mutate(length_group=ifelse(length>=90  & length<105, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=105 & length<120, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=120 & length<135, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=135 & length<150, 5, length_group)) |> 
  mutate(length_group=ifelse(length>=150 & length<165, 6, length_group)) |> 
  mutate(length_group=ifelse(length>=165 & length<180, 7, length_group)) |> 
  mutate(length_group=ifelse(length>=180, 8, length_group)) 

numbers_at_length_other<-numbers_at_length|>
  filter(species==2) |> 
  mutate(length_group=ifelse(length<60, 1, NA)) |> 
  mutate(length_group=ifelse(length>=60  & length<80, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=80 & length<100, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=100 & length<120, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=120 & length<140, 5, length_group)) |> 
  mutate(length_group=ifelse(length>=140, 6, length_group))

numbers_per_length_group<-full_join(numbers_at_length_herring, numbers_at_length_other)|> 
  group_by(species, year, rec_ruhnu, length_group) |> 
  summarise(number_at_length=sum(n)) |> 
  pivot_wider(names_from = rec_ruhnu, values_from=number_at_length) |> 
  arrange(species,year,length_group)
print(n=100, x=numbers_per_length_group)
#View(numbers_per_length_group)

#===============================
# Median lengths in length groups
#===============================
medianL_herring<-medianL_other<-c()

# ========== Herring  =======================
# herring <90: filter small individuals and calculate their median length
medianL_herring<-as.data.frame(dfB_catch|>
              filter(length<90, species==1) |> select(-species) |> 
              summarise(medianL=median(length)))[[1]]
for(i in 1:6){
  medianL_herring[i+1]<-length_limits_herring[i]+
    (length_limits_herring[i+1]-length_limits_herring[i])/2
}


# >180
medianL_herring[8]<-
  as.data.frame(
    dfB_catch|>
      filter(length>180, species==1) |> select(-species) |> 
      summarise(medianL=median(length))
  )[[1]]
medianL_herring

# ========== Other species  =======================

# Just to chekc out: median lengths of different species other than herring
dfB_catch|>
  filter(#length>140, 
    species==2) |> 
  group_by(CatchSpeciesCode) |> 
  summarise(x=median(length), n=n()) |> 
  arrange(x)

# Other species than herring <60mm: filter small individuals 
# and calculate their median length
medianL_other<-as.data.frame(dfB_catch|>
                               filter(length<60, species==2) |> select(-species) |> 
                               summarise(medianL=median(length)))[[1]]
# median lengths in groups 2-5
for(i in 1:4){
  medianL_other[i+1]<-length_limits_other[i]+
    (length_limits_other[i+1]-length_limits_other[i])/2
}

# Last group, individuals >140mm
# DO NOT ACCOUNT FOR LAMBREY IN CALCULATING THE median LENGTH OF THE LAST GROUP!!!
medianL_other[6]<-
  as.data.frame(
    dfB_catch|>
      # 101172 is river lambrey
      filter(length>140, species==2, CatchSpeciesCode!= 101172) |> 
      summarise(medianL=median(length))
  )[[1]]
medianL_other

# ==============================================

# Sample size per species and rec in a form that feeds to the model
# Lobs[1:8,r,s,y]
max_group_num<-numbers_per_length_group |>group_by(species) |> 
  summarise(max=max(length_group))

max_group_num<-max(numbers_per_length_group$length_group)
L_obs<-array(NA, dim=c(max_group_num,4,2,Nyears))
for(y in 1:Nyears){
  for(r in 1:4){
    for(g in 1:8){ # Herring
      L_obs[g,r,1,y]<-as.data.frame(
      numbers_per_length_group |> 
        filter(species==1 & year==(y+min_years-1) & length_group==g) |>
        ungroup() |> select(-year, -species, -length_group))[,r]
    }
    for(g in 1:6){ # Other species
      L_obs[g,r,2,y]<-as.data.frame(
        numbers_per_length_group |> 
          filter(species==2 & year==(y+min_years-1) & length_group==g) |>
          ungroup() |> select(-year, -species, -length_group))[,r]
    }
  }
}

L_obs
nL_obs

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
