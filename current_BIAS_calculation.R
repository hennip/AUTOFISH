source("packages-and-paths.R")

#source("data/read-in-acoustic-data.R") 

# ices.rect2-function example:
# lon <- c(21.97854644, 23.03531506)
# lat <- c(57.76010658, 57.83974327)
# # Convert to ICES rectangles
# rect <- ices.rect2(lon, lat); rect

# BIAS survey data for 2024 => change path
pathA<-pathB<-"../../01-Projects/AUTOFISH/dat/BIAS_24/"
dfA24<-read.csv(str_c(pathA,"Acoustic_ESTBIAS2024_2025-01-03T08.14.20.660.csv"), skip=11) |> 
  as_tibble() |> mutate(year=2024)
source("data/read-in-trawl-data.R") 

# Rectangle specific areas as NM^2
rec_areas<-read_xlsx(str_c("../../01-Projects/AUTOFISH/dat/ICES_rec_areas.xlsx")) 

dfA24
catch_all
hauls_all
bio_all
rec_areas

# Define the year to be investigated
choose_year<-2024

# Modify the datasets: Filter year and transform chr variables to numeric where needed
df_acou<-dfA24 |> filter(year==choose_year)
df_catch<-catch_all|> filter(SurveyYear==choose_year) |> 
  mutate(CatchSpeciesCategoryNumber=as.numeric(CatchSpeciesCategoryNumber),
         CatchNumberAtLength=as.numeric(CatchNumberAtLength),
         CatchLengthClass=as.numeric(CatchLengthClass))
df_hauls<-hauls_all|> filter(SurveyYear==choose_year)
df_bio<-bio_all|> filter(SurveyYear==choose_year)
rec_areas<-rec_areas |> rename(HaulStatisticalRectangle=rec)


# ==============================================================================
# CALCULATIONS 
# ==============================================================================

# Define ICES rectangles and sum over NASC from different depth layers
# Function ices.rect2 is in the mapplots package
df_edsu<-df_acou |> 
  mutate(HaulStatisticalRectangle=ices.rect2(LogLongitude, LogLatitude)) |> 
  select(HaulStatisticalRectangle, everything()) |> 
    group_by(year, HaulStatisticalRectangle, LogDistance) |> 
    summarise(edsu=sum(DataValue)) # elementary distance sampling unit

# Take mean NASC per ICES rectangle
df_nasc<-df_edsu |> 
  group_by(year, HaulStatisticalRectangle) |> 
  summarise(mean_nasc=mean(edsu)) |> 
  left_join(rec_areas)
df_nasc

# Select only needed variables from the haul table
df_haul_rec<-df_hauls |> 
  select(SurveyYear, HaulNumber, HaulStatisticalRectangle)

# (( Calculate percentage of each species in the haul))

# Join rectangles to catch table
df_catch_w_rec<-df_catch |> 
  left_join(df_haul_rec) |> 
  select(HaulStatisticalRectangle,CatchSpeciesCategoryNumber, CatchSpeciesCode,
         everything()) 

# Total catch per haul per species
df_catch_per_species<-df_catch_w_rec |> 
  select(SurveyYear, HaulNumber,HaulStatisticalRectangle, CatchSpeciesCode, CatchSpeciesCategoryNumber) |> 
  distinct() 

# Total catch per haul, all species
df_tot_catch_per_haul<-df_catch_per_species|> 
  group_by(SurveyYear, HaulNumber, HaulStatisticalRectangle) |> 
  summarise(tot_catch=sum(CatchSpeciesCategoryNumber))

# Percentage of each species per rectangle
# Logically this should be calculated by pooling together all hauls in 
# a rectangle, but in the current method the rectangle specific percentage is 
# calculated as mean of haul specific percentages 
df_p_species_per_rectangle<-df_catch_per_species  |> 
  left_join(tot_catch_per_haul) |> 
  mutate(p_species=CatchSpeciesCategoryNumber/tot_catch*100) |> 
  group_by(SurveyYear, HaulStatisticalRectangle, CatchSpeciesCode) |> 
  summarise(p_species_per_rec=mean(p_species)) # NOTE! THIS GIVES EQUAL WEIGHTS FOR HAULS OF DIFFERENT SIZE!!!
print(x=df_p_species_per_rectangle, n=100)

# Just to check these sum to 100
df_p_species_per_rectangle |> summarise(sum=sum(p_species_per_rec))

# Catch sample sizes per length 
df_sample_size_per_length<-df_catch_w_rec |> 
  group_by(SurveyYear, CatchSpeciesCode, HaulNumber) |> 
  summarise(sample_size_per_length=sum(CatchNumberAtLength))

# Join sample sizes per length with catch table and calculate percentage of
# individuals in a specific length class
df_p_per_length<-df_catch_w_rec |> 
  left_join(df_sample_size_per_length) |> 
  mutate(p_per_length=CatchNumberAtLength/sample_size_per_length*100) |> 
  select(SurveyYear, HaulStatisticalRectangle, HaulNumber, CatchLengthClass, p_per_length, everything()) 
df_p_per_length

# Multiply the precentage of species at certain length per haul with the share 
# of that species in the haul
# ==============================

# On each line of total catch per haul per species, add total catch per haul and
# calculate the proportion of each species per haul
df_p_species_per_haul<-df_catch_per_species  |> 
  left_join(tot_catch_per_haul) |> 
  mutate(p_species_per_haul=CatchSpeciesCategoryNumber/tot_catch) 
print(x=df_p_species_per_haul, n=100)

# pp is the product of the proportion of a species and the percentage per length
df_pp<-df_p_per_length |> full_join(df_p_species_per_haul) |> 
  mutate(pp=p_per_length*p_species_per_haul) |> 
  select(pp, everything())
df_pp

# Check that it sums up to 100
print(df_pp |> group_by(HaulNumber) |> summarise(sumx=sum(pp)),
      n=50)

# For each row in the df_pp, calculate sigma
df_sigma<-df_pp |> 
  mutate(d=9.5325669476e-07) |> # d is the same for all clupeids
#  mutate(d=ifelse(CatchSpeciesCode==xxxx, 9.1, d) # Example on how to change d for a specific species xxxx
  mutate(sigma=d*(CatchLengthClass/10+0.2)^2) |>  # sigma=d*(L_cm+offset_cm)^2
  select(sigma, everything())
#View(df_sigma)  

# For each row, calculate product of sigma and pp
# Then group by haul and rec and calculate haul specific sigma
df_sigma_haul<-df_sigma |> 
  mutate(sigma_x_pp=sigma*pp) |> select(sigma_x_pp, everything()) |> 
  group_by(HaulNumber, HaulStatisticalRectangle) |> 
  summarise(sigma_haul=sum(sigma_x_pp)/sum(pp)) |> 
  select(sigma_haul, everything())
#View(df_sigma_haul)

# Rectangle specific sigma is calculated in this method
# as a mean of haul specific sigmas of that rectangle 
df_sigma_rectangle <- df_sigma_haul |> 
  group_by(HaulStatisticalRectangle) |> 
  summarise(sigma_rectangle=mean(sigma_haul)) 
df_sigma_rectangle
  
# Join rectangle specific mean nasc and the sigma values
# filter out rectangles for which sigma is missing (no hauls)
df_nasc_per_rectangle<-df_nasc |> 
  left_join(df_sigma_rectangle) |> 
  filter(is.na(sigma_rectangle)==F)

# Pivot table: percentage of each species per rectangle
pivot_p_per_species<-df_p_species_per_rectangle |> ungroup() |> 
  select(CatchSpeciesCode, HaulStatisticalRectangle, p_species_per_rec) |> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = p_species_per_rec) 
pivot_p_per_species

# Join percentage of species per rectangle (unpivoted) with other
# rectangle specific data
# Calculate number of individuals per species per NM2, 
# total number of individuals and 
# the number of individuals per species per rectangle  
df_species<-full_join(df_nasc_per_rectangle, df_p_species_per_rectangle) |> 
  select(-HaulStatisticalRectangle) |> 
  mutate(N_per_NM2=mean_nasc/sigma_rectangle/1000000,
         Ntot=N_per_NM2*A_NM2) |> 
  select(year, rec, N_per_NM2, Ntot, everything()) |> 
  mutate(n_per_species_per_rectangle=p_species_per_rec/100*Ntot)
print(df_species, n=100)

# Pivot: Number of individuals per species per rectangle
df_species |>   
  select(-SurveyYear, -p_species_per_rec) |> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = n_per_species_per_rectangle)



