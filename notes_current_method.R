source("packages-and-paths.R")

source("data/read-in-acoustic-data.R") 
source("data/read-in-trawl-data.R") 

library(mapplots)
# # Example coordinates
# lon <- c(21.97854644, 23.03531506)
# lat <- c(57.76010658, 57.83974327)
# # Convert to ICES rectangles
# rect <- ices.rect2(lon, lat)
# # rect

# BIAS survey data for 2024 => change path
pathA<-pathB<-"../../01-Projects/AUTOFISH/dat/BIAS_24/"
dfA24<-read.csv(str_c(pathA,"Acoustic_ESTBIAS2024_2025-01-03T08.14.20.660.csv"), skip=11) |> as_tibble() |> mutate(year=2024)
source("data/read-in-trawl-data.R") 

rec_areas<-read_xlsx(str_c("../../01-Projects/AUTOFISH/dat/ICES_rec_areas.xlsx")) 



dfA24
catch_all
hauls_all
bio_all

choose_year<-2024

df_acou<-dfA24 |> filter(year==choose_year)
df_catch<-catch_all|> filter(SurveyYear==choose_year) |> 
  mutate(CatchSpeciesCategoryNumber=as.numeric(CatchSpeciesCategoryNumber),
         CatchNumberAtLength=as.numeric(CatchNumberAtLength))
df_hauls<-hauls_all|> filter(SurveyYear==choose_year)
df_bio<-bio_all|> filter(SurveyYear==choose_year)

# First define the ICES rectangles and then sum over NASC from different depth layers
df_edsu<-df_acou |> mutate(rec=ices.rect2(LogLongitude, LogLatitude)) |> 
  select(rec, everything()) |> 
    group_by(year, rec, LogDistance) |> 
    summarise(edsu=sum(DataValue)) # elementary distance sampling unit

# Take mean NASC per ICES rectangle
df_nasc<-df_edsu |> group_by(year, rec) |> 
  summarise(mean_nasc=mean(edsu)) |> 
  left_join(rec_areas)|> 
  mutate(HaulStatisticalRectangle=rec)
  df_nasc

df_haul_rec<-df_hauls |> select(SurveyYear, HaulNumber, HaulStatisticalRectangle)

# Calculate percentage of each species in the haul

df3<-df_catch |> left_join(df_haul_rec) |> select(HaulStatisticalRectangle, everything()) |> 
  select(CatchSpeciesCategoryNumber, CatchSpeciesCode, everything())

df4<-df3 |> 
  select(CatchSpeciesCategoryNumber, CatchSpeciesCode, SurveyYear, HaulNumber,HaulStatisticalRectangle) |> 
  distinct() 

tot_catch_per_haul<-df4|> 
  group_by(SurveyYear, HaulNumber, HaulStatisticalRectangle) |> 
  summarise(tot_catch=sum(CatchSpeciesCategoryNumber))

p_species_per_rectangle<-df4  |> left_join(tot_catch_per_haul) |> 
  mutate(p_haul=CatchSpeciesCategoryNumber/tot_catch*100) |> 
  group_by(SurveyYear, CatchSpeciesCode,HaulStatisticalRectangle) |> 
  summarise(p_species_per_rec=mean(p_haul)) # NOTE! THIS GIVES EQUAL WEIGHTS FOR HAULS OF DIFFERENT SIZE!!!
print(x=p_species_per_rectangle, n=100)

sample_size_per_length<-df3 |> 
  group_by(SurveyYear, CatchSpeciesCode, HaulNumber) |> 
  summarise(sample_size_per_length=sum(CatchNumberAtLength))

per_length<-df3 |> left_join(sample_size_per_length) |> 
  mutate(p_per_length=CatchNumberAtLength/sample_size_per_length*100) |> 
  select(p_per_length, CatchLengthClass,everything()) |> 
  select(SurveyYear, HaulStatisticalRectangle, HaulNumber, everything()) |> 
  mutate(CatchLengthClass=as.numeric(CatchLengthClass)) 

# multiply the precentage of species at certain length per haul with the share 
# of that species in the haul

# percentage of species at certain length is 
per_length

# share of each species in the haul is
df_p_species_per_haul<-df4  |> left_join(tot_catch_per_haul) |> 
  mutate(p_species_per_haul=CatchSpeciesCategoryNumber/tot_catch) 
print(x=df_p_species_per_haul, n=100)

df_pp<-per_length |> full_join(df_p_species_per_haul) |> 
  mutate(pp=p_per_length*p_species_per_haul) |> 
  select(pp, everything())
View(df_pp)

# Check it sums up to 100
tmp<-df_pp |> group_by(HaulNumber) |> summarise(sumx=sum(x))
print(tmp, n=50)

df_sigma<-df_pp |> mutate(d=9.5325669476e-07) |> # d is the same for all clupeids
#  mutate(d=ifelse(CatchSpeciesCode==xxxx, 9.1, d) # Example on how to change d for a specific species xxxx
  mutate(sigma=d*(CatchLengthClass/10+0.2)^2) |>  # sigma=d*(L_cm+offset_cm)^2
select(sigma, everything())
View(df_sigma)  

df_sigma_haul<-df_sigma |> mutate(sigma_x_pp=sigma*pp) |> select(sigma_x_pp, everything()) |> 
  group_by(HaulNumber, HaulStatisticalRectangle) |> 
  summarise(sigma_haul=sum(sigma_x_pp)/sum(pp)) |> select(sigma_haul, everything())
View(df_sigma_haul)

df_sigma_rectangle <- df_sigma_haul |> group_by(HaulStatisticalRectangle) |> 
  summarise(sigma_rectangle=mean(sigma_haul)) 
df_sigma_rectangle
  



df_nasc_per_rectangle<-df_nasc |> left_join(df_sigma_rectangle) |> 
  filter(is.na(sigma_rectangle)==F) #|> 
  


pivot_p_per_species<-p_species_per_rectangle |> ungroup() |> 
  select(CatchSpeciesCode, HaulStatisticalRectangle, p_species_per_rec) |> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = p_species_per_rec) 


#full_join(df_nasc_per_rectangle, pivot_p_per_species) |> 
df_species<-full_join(df_nasc_per_rectangle, p_species_per_rectangle) |> 
  select(-HaulStatisticalRectangle) |> 
  mutate(N_per_NM2=mean_nasc/sigma_rectangle/1000000,
         Ntot=N_per_NM2*A_NM2) |> 
  select(year, rec, N_per_NM2, Ntot, everything()) |> 
  mutate(n_per_species_per_rectangle=p_species_per_rec/100*Ntot)
View(df_species)

df_species |>   select(-SurveyYear, -p_species_per_rec) |> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = n_per_species_per_rectangle)





  
#  df |> select(rec, LogDistance,DataValue)

# FINAL BIAS excel
# species weights per trawl haul
# mean weight of the species per trawl haul
# number of individuals per species per trawl haul (herrin, sprat, stickleback, smelt)
# % of species at the haul, calculated from the numbers
# sheet: Traalid




# Pikkusruhmad sheet: length groups

# % of length group per species 
#koossels sheet

# sigma
# sigma of an individual of specific length
# results with the sigma of the most average individual in the trawl haul


# Nearest excel
# T01 ect, distance of each trawl haul to each NASC
#nearest trawl haul -> min
# total number of fish per NM2 in that nasc log (AI column)
# choosing the nearest trawl haul and dividing the nasc into species
# million herring per NM2 and sprat and so on
