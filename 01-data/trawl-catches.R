###############
# Catch data
###############

dfB_catch<-catch_all |> mutate(year=SurveyYear)|> 
  filter(year>=min_year & year<=max_year) |> 
  mutate(CatchNumberAtLength=as.numeric(CatchNumberAtLength)) |> 
  mutate(length=as.numeric(CatchLengthClass)) |> 
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> # Note that this contains doubles!
  mutate(catch=round(catch,0)) |> 
  #mutate(species=ifelse(CatchSpeciesCode==126417,1,2))  |> # 1: Herring, 2:other
  # mutate(species=ifelse(CatchSpeciesCode==126417,1,
  #                       ifelse(CatchSpeciesCode==126425,2,3))) |>  # 1: Herring, 2:sprat, 3:other
  mutate(species=ifelse(CatchSpeciesCode==126417,1,
                         ifelse(CatchSpeciesCode==126425,2,
                                ifelse(CatchSpeciesCode==126505,3,4)))) |>  # 1: Herring, 2:sprat, 3:stickleback, 4:other
  full_join(df_rec) |> 
  select(year,rec_ruhnu, everything())|> 
  select(-CatchDataType, -CatchSpeciesValidity)


# The catch data is a combination of two datasets:
# 1: Total catch, separated to species
# 2: Catch sample data with measured lengths
# Because these two are combined, CatchNumberAtLength contains duplicates of
# species specific catches.
  
# Thus for the sake of clarity, split the data in two versions:
# dfB_catch_all, containing data only about species specific total catches
# dfB_catch_sample, containing data on the length samples

dfB_catch_all<-dfB_catch |> 
  select(year, rec_ruhnu, HaulNumber, CatchSpeciesCode, species, catch) |> 
  distinct()
#dfB_catch_all

# Sum together species other than herring or sprat
dfB_catch_all_species<-dfB_catch_all |> 
  group_by(year, rec_ruhnu, HaulNumber, species) |> 
  summarise(catch3=sum(catch))
  
dfB_catch_sample<-dfB_catch |> 
  select(year, rec_ruhnu, HaulNumber, CatchSpeciesCode, species, length, CatchNumberAtLength) 
#dfB_catch_sample

dfB_catch_all |> 
       group_by(year, species) |> 
       summarise(catchTOT=sum(catch))


# Cobs: total catch per trawl haul
#########################################
# group by rec & haul, calculate total catch
TotCatch<-dfB_catch_all_species |>
  group_by(year,rec_ruhnu,HaulNumber) |> 
  summarise(tot_catch=sum(catch3, na.rm=T))#|> 
  #select(year,rec_ruhnu, HaulNumber, tot_catch)
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


# Sobs: Number of individuals per species in the catch
# Sobs[s,h,r,y]
###############################################
# Species:
# 1: herring
# 2: sprat
# 3: stickleback
# 3: ther
# group by rec, haul & species, calculate total catch
Nspecies<-4

S_obs<-array(NA, dim=c(Nspecies, max_number_of_hauls,4,Nyears))

for(y in 1:Nyears){
  for(r in 1:4){
    dat<-dfB_catch_all_species |> 
      filter(year==(y+min_years-1) &rec_ruhnu ==r)
    df<-t(as.data.frame(dat |> pivot_wider(names_from = species, values_from = catch3) |> 
                  #filter(year==2023, rec_ruhnu==1) |> 
                    ungroup() |>  select(-year, -rec_ruhnu, -HaulNumber)))
S_obs[,1:dim(df)[2],r,y]<-df
}}
S_obs

# Replace NA's with 0 in cases where haul took place but
# did not contain the particular species
for(y in 1:Nyears){
  for(r in 1:4){
    for(h in 1:max_number_of_hauls){
      for(s in 1:Nspecies){
        if(is.na(C_obs[h,r,y])==F&
          is.na(S_obs[s,h,r,y])==T){
          S_obs[s,h,r,y]<-0
        }
      }
    }
  }
}
S_obs

# If Hobsprop will be used later, please notice that the data contains duplicates
# Probably doesn't matter because of the proportions, but should anyways be cleaned up
# # Hobsprop: Proportion of herring in each catch
# ###############################################
# # group by rec, haul & species, calculate total catch
# dfB_herring<-catch_all %>% mutate(year=SurveyYear)|> 
#   filter(year>=min_year & year<=max_year)|>
#   mutate(HaulNumber=as.numeric(HaulNumber)) |> 
#   mutate(catch=as.numeric(CatchSpeciesCategoryNumber)) |> 
#   mutate(catch=round(catch,0)) |> 
#   mutate(CatchNumberAtLength=as.numeric(CatchNumberAtLength)) |> 
#   mutate(length=as.numeric(CatchLengthClass)) |> 
#   full_join(df_rec) |> 
#   mutate(species=ifelse(CatchSpeciesCode==126417,1,2))  |> # 1: Herring, 2:other
#   select(year,rec_ruhnu, everything())|> 
#   #select(-Catch, -Header, -HaulGear, -CruiseLocalID)|> 
#   select(-CatchDataType, -CatchSpeciesValidity)
# dfB_herring
# 
# herring<-dfB_catch  |> 
#   group_by(year,rec_ruhnu,HaulNumber, species) |> 
#   summarise(tot_catch=sum(catch))|> 
#   select(year,rec_ruhnu, HaulNumber, species, tot_catch) |> 
#   filter(species==1) |>  # herring only
#   mutate(herring_catch=tot_catch) |> 
#   select(-tot_catch)
# herring
# 
# dfH<-full_join(herring, TotCatch) |> 
#   mutate(hprop=herring_catch/tot_catch)

# 
# #HobsProp[h,r,y]
# # Build table for herring proportions in which rows are hauls and columns are rectangles
# Hprops<-array(NA, dim=c(max_number_of_hauls,4,Nyears))
# for(y in 1:Nyears){
#   for(r in 1:4){
#     apu<-1
#     for(i in 1:length(dfH$rec_ruhnu)){
#       if(dfH$year[i]==(y+min_years-1) & dfH$rec_ruhnu[i]==r){
#         Hprops[apu,r,y]<-dfH$hprop[i]
#         apu<-apu+1
#       }}}}
# Hprops
# 

# nLobs[r,s,y]: Total sample size per rectangle and species
# Lobs[1:8,r,s,y]: Number of fish of species s in all haul samples at rectangle r from length groups 1:8
#######################################################################################
# nLobs
#==========================
# catch sample size per ruhnu rectangle and species (1=herring, 2=other)
sample_size<-dfB_catch_sample  |>
  group_by(year,rec_ruhnu,species) |> 
  summarise(tot_sample=sum(CatchNumberAtLength))#|> 
sample_size

# Sample size per species and rec in a form that feeds to the model
nL_obs<-array(NA, dim=c(4,Nspecies,Nyears))
for(y in 1:Nyears){
  nL_obs[,1,y]<-as.data.frame(sample_size |> filter(year==(y+min_years-1), species==1) |>  
                                pivot_wider(values_from = tot_sample, names_from = species))[,3]
  nL_obs[,2,y]<-as.data.frame(sample_size |> filter(year==(y+min_years-1), species==2) |>  
                                pivot_wider(values_from = tot_sample, names_from = species))[,3]
  nL_obs[,4,y]<-as.data.frame(sample_size |> filter(year==(y+min_years-1), species==4) |>  
                                pivot_wider(values_from = tot_sample, names_from = species))[,3]
  nL_obs[,3,y]<-as.data.frame(sample_size |> filter(year==(y+min_years-1), species==3) |>  
                                pivot_wider(values_from = tot_sample, names_from = species))[,3]
}
nL_obs

# Lobs 
#==========================
# min and max length per species
print(x=dfB_catch_sample |>group_by(CatchSpeciesCode) |>  
  summarise(median= median(length),min=min(length), max=max(length)), n=100)

  # Define length groups per species and calculate
# number of individuals in each group
# NOTE! Number of groups differs for different species

numbers_at_length<-dfB_catch_sample  |> 
  group_by(year,rec_ruhnu, species, length)|>  
summarise(n=sum(CatchNumberAtLength)) 
numbers_at_length
#View(numbers_at_length)

#length_limits_herring<-c(90,105,120,135,150,165,180) # 8 groups for herring
#length_limits_sprat<-c(70,90,110,130) # 5 groups for sprat
#length_limits_other<-c(60,80,100,120,140,160, 180) # 8 groups for other species

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

numbers_at_length_sprat<-numbers_at_length|>
  filter(species==2) |> 
  mutate(length_group=ifelse(length<70, 1, NA)) |> 
  mutate(length_group=ifelse(length>=70  & length<90, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=90 & length<110, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=110 & length<130, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=130, 5, length_group)) 

numbers_at_length_stickl<-numbers_at_length|>
  filter(species==3) |> 
  mutate(length_group=ifelse(length<50, 1, NA)) |> 
  mutate(length_group=ifelse(length>=50  & length<55, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=55 & length<60, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=60, 4, length_group)) 


numbers_at_length_other<-numbers_at_length|>
  #filter(species==2) |>  # If two species
  filter(species==4) |>  # If three species
  mutate(length_group=ifelse(length<60, 1, NA)) |> 
  mutate(length_group=ifelse(length>=60  & length<80, 2, length_group)) |> 
  mutate(length_group=ifelse(length>=80 & length<100, 3, length_group)) |> 
  mutate(length_group=ifelse(length>=100 & length<120, 4, length_group)) |> 
  mutate(length_group=ifelse(length>=120 & length<140, 5, length_group)) |> 
  mutate(length_group=ifelse(length>=140 & length<160, 6, length_group)) |> 
  mutate(length_group=ifelse(length>=160 & length<180, 7, length_group)) |> 
  mutate(length_group=ifelse(length>=180, 8, length_group))

numbers_per_length_group<-full_join(numbers_at_length_herring, 
                                    numbers_at_length_sprat) |> 
  full_join(numbers_at_length_stickl)|> 
  full_join(numbers_at_length_other)|> 
  group_by(species, year, rec_ruhnu, length_group) |> 
  summarise(number_at_length=sum(n)) |> 
  pivot_wider(names_from = rec_ruhnu, values_from=number_at_length) |> 
  arrange(species,year,length_group)
print(n=100, x=numbers_per_length_group)
#View(numbers_per_length_group)

#===============================
# Median lengths in length groups
#===============================
# check out histograms on length
# summary(filter(dfB_catch, species==2) |> select(length) )
# df_h<-filter(dfB_catch, species==1) |> select(length)
# df_s<-filter(dfB_catch, species==2) |> select(length)
# df_o<-filter(dfB_catch, species==3) |> select(length)
#ggplot(df_o, aes(x=length))+
#  geom_histogram()


# ========== Herring  =======================
medianL_herring<-c()
N_lh<-length(length_limits_herring)+1 # Number of length groups

# Smallest group: filter small individuals and calculate their median length
medianL_herring<-as.data.frame(dfB_catch|>
              filter(length<length_limits_herring[1], species==1) |> 
                select(-species) |> 
              summarise(medianL=median(length)))[[1]]
for(i in 1:(N_lh-2)){ # groups 2-7
  medianL_herring[i+1]<-length_limits_herring[i]+
    (length_limits_herring[i+1]-length_limits_herring[i])/2
}


# Largest group
medianL_herring[N_lh]<-
  as.data.frame(
    dfB_catch|>
      filter(length>length_limits_herring[N_lh-1], species==1) |> 
      select(-species) |> 
      summarise(medianL=median(length))
  )[[1]]
medianL_herring

# ========== Sprat  =======================

# Just to check out: median lengths of sprat
dfB_catch|>
  filter(#length>140, 
    species==2) |> 
  group_by(length) |> 
  summarise(n=n()) 


N_lsprat<-length(length_limits_sprat)+1 # Number of length groups
medianL_sprat<-c()

# Smallest group: filter small individuals and calculate their median length
medianL_sprat<-as.data.frame(dfB_catch|>
                  filter(length<length_limits_sprat[1], species==2) |> 
                                 select(-species) |> 
                                 summarise(medianL=median(length)))[[1]]
for(i in 1:(N_lsprat-2)){ 
  medianL_sprat[i+1]<-length_limits_sprat[i]+
    (length_limits_sprat[i+1]-length_limits_sprat[i])/2
}


# Largest group
medianL_sprat[N_lsprat]<-
  as.data.frame(
    dfB_catch|>
      filter(length>length_limits_sprat[N_lsprat-1], species==2) |> 
      select(-species) |> 
      summarise(medianL=median(length))
  )[[1]]
medianL_sprat

# ========== Stickleback  =======================

# Just to check out: median lengths of stickleback
dfB_catch|>
  filter(
    species==3) |> 
  group_by(length) |> 
  summarise(n=n()) 


N_lstickl<-length(length_limits_stickl)+1 # Number of length groups
medianL_stickl<-c()

# Smallest group: filter small individuals and calculate their median length
medianL_stickl<-as.data.frame(dfB_catch|>
                               filter(length<length_limits_stickl[1], species==3) |> 
                               select(-species) |> 
                               summarise(medianL=median(length)))[[1]]
for(i in 1:(N_lstickl-2)){ 
  medianL_stickl[i+1]<-length_limits_stickl[i]+
    (length_limits_stickl[i+1]-length_limits_stickl[i])/2
}


# Largest group
medianL_stickl[N_lstickl]<-
  as.data.frame(
    dfB_catch|>
      filter(length>length_limits_stickl[N_lstickl-1], species==3) |> 
      select(-species) |> 
      summarise(medianL=median(length))
  )[[1]]
medianL_stickl


# ========== Other species  =======================
# Just to check out: median lengths of different species other than herring
dfB_catch|>
  filter( 
    species==4) |> 
  group_by(CatchSpeciesCode) |> 
  summarise(x=median(length), n=n()) |> 
  arrange(x)

N_lo<-length(length_limits_other)+1 # Number of length groups
medianL_other<-c()

# Smallest group: filter small individuals and calculate their median length
medianL_other<-as.data.frame(dfB_catch|>
                  filter(length<length_limits_other[1], species==4) |> 
                               select(-species) |> 
                               summarise(medianL=median(length)))[[1]]
for(i in 1:(N_lo-2)){ 
  medianL_other[i+1]<-length_limits_other[i]+
    (length_limits_other[i+1]-length_limits_other[i])/2
}


# Largest group
medianL_other[N_lo]<-
  as.data.frame(
    dfB_catch|>
      filter(length>length_limits_other[N_lo-1], species==4) |> 
      select(-species) |> 
      summarise(medianL=median(length))
  )[[1]]
medianL_other
length_limits_other

# Vector for mid lengths is called meanL, although the points are medians
meanL<-array(NA, dim=c(8,Nspecies))
meanL[,1]<-medianL_herring
meanL[1:N_lsprat,2]<-medianL_sprat
meanL[1:N_lstickl,3]<-medianL_stickl
meanL[1:N_lo,4]<-medianL_other
meanL


# ==============================================

# Sample size per species and rec in a form that feeds to the model
# Lobs[1:8,r,s,y]

numbers_per_length_group |>group_by(species) |> 
  summarise(max=max(length_group))

# If there's no individuals in a case, replace 0
# Need to check though that the NA's are still ok
max_group_num<-max(numbers_per_length_group$length_group)
L_obs<-array(NA, dim=c(max_group_num,4,Nspecies,Nyears))
for(y in 1:Nyears){
  for(r in 1:4){
      for(g in 1:N_lh){ # Herring
      tmp<-numbers_per_length_group |> 
        filter(species==1 & year==(y+min_years-1) & length_group==g)
      L_obs[g,r,1,y]<-ifelse(is.null(tmp)==F,
                             as.data.frame(tmp|>
        ungroup() |> select(-year, -species, -length_group))[,r],0)
    }
    for(g in 1:N_lsprat){ # Sprat
      tmp<-numbers_per_length_group |> 
        filter(species==2 & year==(y+min_years-1) & length_group==g)
      L_obs[g,r,2,y]<-ifelse(is.null(tmp)==F,
                             as.data.frame(tmp|>
        ungroup() |> select(-year, -species, -length_group))[,r],0)
    }
    for(g in 1:N_lstickl){ # Stickleback
      tmp<-numbers_per_length_group |> 
        filter(species==3 & year==(y+min_years-1) & length_group==g)
      L_obs[g,r,3,y]<-ifelse(is.null(tmp)==F,
                             as.data.frame(tmp|>
                                             ungroup() |> select(-year, -species, -length_group))[,r],0)
    }
    for(g in 1:N_lo){ # Other species
      tmp<-numbers_per_length_group |> 
        filter(species==4 & year==(y+min_years-1) & length_group==g)
      L_obs[g,r,4,y]<-ifelse(is.null(tmp)==F,
                             as.data.frame(tmp|>
        ungroup() |> select(-year, -species, -length_group))[,r],0)
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
  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      if(is.na(nL_obs[r,s,y])==T){
        nL_obs[r,s,y]<-500}else{ # Input imaginary 500 sample where no sample was taken
          for(l in 1:N_lh){ # Herring
            if(is.na(L_obs[l,r,1,y])==T){
              L_obs[l,r,1,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
            }
          }
          for(l in 1:N_lsprat){ # Sprat
            if(is.na(L_obs[l,r,2,y])==T){
              L_obs[l,r,2,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
            }
          }
          for(l in 1:N_lstickl){ # Stickleback
            if(is.na(L_obs[l,r,3,y])==T){
              L_obs[l,r,3,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
            }
          }
          for(l in 1:N_lo){ # Other
            if(is.na(L_obs[l,r,4,y])==T){
              L_obs[l,r,4,y]<-0 # Input zero when sample size is not NA but none was observed (==real 0s)
            }
          }
        }
      }
    }
  }
L_obs
nL_obs

