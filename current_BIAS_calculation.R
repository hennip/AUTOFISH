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

df_hauls_rec<-hauls_all|> 
  filter(SurveyYear==choose_year)|> 
  mutate(rec=HaulStatisticalRectangle) |> select(-HaulStatisticalRectangle) |> 
  select(SurveyYear,HaulNumber,rec)

df_n_hauls_per_rec<-df_hauls_rec |> group_by(rec) |> 
  summarise(n_hauls_per_rec=n())

df_catch<-catch_all|> filter(SurveyYear==choose_year) |> 
  mutate(CatchSpeciesCategoryNumber=as.numeric(CatchSpeciesCategoryNumber),
         CatchNumberAtLength=as.numeric(CatchNumberAtLength),
         CatchLengthClass=as.numeric(CatchLengthClass),
         CatchWeightAtLength=as.numeric(CatchWeightAtLength),
         CatchNumberAtLength=as.numeric(CatchNumberAtLength))

# Join rectangles to catch table
df_catch_w_rec<-df_catch |> 
  left_join(df_hauls_rec) |> 
  select(rec,CatchSpeciesCode,everything()) 

# # täytä puuttuvat nollat 
# df2<-df_catch_w_rec |> select(HaulStatisticalRectangle,HaulNumber, 
#                           CatchSpeciesCode,CatchLengthClass, CatchNumberAtLength)
# min(df2$CatchLengthClass)
# max(df2$CatchLengthClass)
# df3<-df2 |> pivot_wider(names_from = CatchLengthClass, values_from = CatchNumberAtLength)
# df3 |> pivot
# df3 |> replace_na()

df_bio<-bio_all|> filter(SurveyYear==choose_year) |> 
  mutate(BiologyLengthClass =as.numeric(BiologyLengthClass ))

rec_areas<-rec_areas 


# ==============================================================================
# CALCULATIONS --- CALL THIS SOMETHING ELSE
# ==============================================================================

# Define ICES rectangles for each data point 
# and sum over NASC from different depth layers
# Function ices.rect2 is in the mapplots package
df_edsu<-df_acou |> 
  mutate(rec=ices.rect2(LogLongitude, LogLatitude)) |> 
  select(rec, everything()) |> 
  group_by(year, rec, LogDistance) |> 
  summarise(edsu=sum(DataValue)) # elementary distance sampling unit

# Take mean NASC per ICES rectangle
df_nasc<-df_edsu |> 
  group_by(year, rec) |> 
  summarise(mean_nasc=mean(edsu)) |> 
  left_join(rec_areas)
df_nasc


# Basic data wrangling
# ==============================================================================

# Catch sample sizes per length per haul
df_sample_size_per_length<-df_catch_w_rec |> 
  group_by(SurveyYear, CatchSpeciesCode, HaulNumber) |> 
  summarise(sample_size_per_length=sum(CatchNumberAtLength))

# Join sample sizes per length with the catch table 
# and calculate the percentage of individuals in a specific length class
df_p_per_length<-df_catch_w_rec |> 
  left_join(df_sample_size_per_length) |> 
  mutate(p_per_length=CatchNumberAtLength/sample_size_per_length*100) |> 
  select(SurveyYear, rec, HaulNumber, CatchSpeciesCode,CatchLengthClass, p_per_length, everything()) 
df_p_per_length
#View(df_p_per_length)
write_xlsx(df_p_per_length, "../p_per_length.xlsx")

# THIS JUST INTERESTING TO SEE?
tmp<-df_p_per_length |> 
  group_by(rec,HaulNumber, CatchSpeciesCode) |> 
  summarise(sum=sum(p_per_length))
#View(tmp)

# Number of hauls per rectangle and per species
df_n_hauls_per_case<- df_p_per_length|>
  group_by(rec,CatchSpeciesCode) |>
  summarise(n_hauls_per_case=n_distinct(HaulNumber))
print(x=df_n_hauls_per_case, n=100)

# Rectangle specific proportion of individuals of certain length is the
# mean over length class specific percentages
df_p_per_length_per_rec<-df_p_per_length |>
  group_by(SurveyYear, rec, CatchSpeciesCode, CatchLengthClass) |>
  summarise(sum_p_per_length_per_rec=sum(p_per_length)) |>
  left_join(df_n_hauls_per_case) |>
  mutate(mean_p_per_length_per_rec=sum_p_per_length_per_rec/n_hauls_per_case)
write_xlsx(df_p_per_length_per_rec, "../df_p_per_length_per_rec.xlsx")

# check that all sum to 100
tmp<-df_p_per_length_per_rec|>
  summarise(sum_p=sum(mean_p_per_length_per_rec))
print(x=tmp, n=100)


# Haul specific 
# ==============
# Total catch per haul per species
df_catch_per_species<-df_catch_w_rec |> 
  select(SurveyYear, HaulNumber,rec, CatchSpeciesCode, CatchSpeciesCategoryNumber) |> 
  distinct() 

# Total catch per haul, all species
df_tot_catch_per_haul<-df_catch_per_species|> 
  group_by(SurveyYear, HaulNumber, rec) |> 
  summarise(tot_catch_per_haul=sum(CatchSpeciesCategoryNumber))

# Proportion of each species per haul
df_p_species_per_haul<-df_catch_per_species  |> 
  left_join(df_tot_catch_per_haul) |> 
  mutate(p_species_per_haul=CatchSpeciesCategoryNumber/tot_catch_per_haul) 
#print(x=df_p_species_per_haul, n=100)

# Rectangle specific 
# ==============
# Total catch per rectangle, all species
df_tot_catch_per_rec<-df_catch_per_species|> 
  group_by(SurveyYear, rec) |> 
  summarise(tot_catch_per_rec=sum(CatchSpeciesCategoryNumber))


# ==============================================================================
# Black magic begins here
# ==============================================================================

# Percentage of each species per rectangle
# ==============================================================================
# Logically this should be calculated by pooling together all hauls in 
# a rectangle, but in the current method the rectangle specific percentage is 
# calculated as mean of haul specific percentages 
df_p_species_per_rectangle<-df_catch_per_species  |>
  left_join(df_tot_catch_per_haul) |>
  mutate(p_species=CatchSpeciesCategoryNumber/tot_catch_per_haul*100) |>
  group_by(SurveyYear, rec, CatchSpeciesCode) |>
  summarise(p_species_per_rec=mean(p_species)) # NOTE! THIS GIVES EQUAL WEIGHTS FOR HAULS OF DIFFERENT SIZE!!!
print(x=df_p_species_per_rectangle, n=100)

# Just to check these sum to 100
df_p_species_per_rectangle |> summarise(sum=sum(p_species_per_rec))

# pp is haul specific
# pp is the product of the proportion of a species
# and the percentage of species at certain length 
df_pp<-df_p_per_length |> full_join(df_p_species_per_haul) |> 
  mutate(pp=p_per_length*p_species_per_haul) |> 
  select(pp, everything())
df_pp

# Check that pp's sums up to 100
print(df_pp |> group_by(HaulNumber) |> summarise(sumx=sum(pp)),
      n=50)

# ================================
# SIGMA CALCULATIONS
# ================================

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
  group_by(HaulNumber, rec) |> 
  summarise(sigma_haul=sum(sigma_x_pp)/sum(pp)) |> 
  select(sigma_haul, everything())
#View(df_sigma_haul)

# Rectangle specific sigma is calculated in this method
# as a mean of haul specific sigmas of that rectangle 
df_sigma_rectangle <- df_sigma_haul |> 
  group_by(rec) |> 
  summarise(sigma_rectangle=mean(sigma_haul)) 
df_sigma_rectangle

# ================================
# COMBINE SIGMA WITH NASC
# ================================

# Join rectangle specific mean nasc and the sigma values
# filter out rectangles for which sigma is missing (no hauls)
df_nasc_per_rectangle<-df_nasc |> 
  left_join(df_sigma_rectangle) |> 
  filter(is.na(sigma_rectangle)==F)

# Pivot table: percentage of each species per rectangle
pivot_p_per_species<-df_p_species_per_rectangle |> ungroup() |> 
  select(CatchSpeciesCode, rec, p_species_per_rec) |> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = p_species_per_rec) 
pivot_p_per_species

# Join percentage of species per rectangle (unpivoted) with the other
# rectangle specific data
# Calculate number of individuals per species per NM2, 
# total number of individuals and 
# the number of individuals per species per rectangle  
df_species<-full_join(df_nasc_per_rectangle, df_p_species_per_rectangle) |> 
  mutate(N_per_NM2=mean_nasc/sigma_rectangle/1000000,
         Ntot=N_per_NM2*A_NM2) |> 
  select(year, rec, N_per_NM2, Ntot, everything()) |> 
  mutate(n_per_species_per_rectangle=p_species_per_rec/100*Ntot)
print(df_species, n=100)
#View(df_species)

# Pivot: Number of individuals per species per rectangle
df_species |>   
  select(-SurveyYear, -p_species_per_rec) |> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = n_per_species_per_rectangle)

# ================================
# LENGTH SPECIFIC ABUNDANCES
# ================================

# Following the excel way of doing, assume all hauls have equal weights
# regardless of the sample size 

# take first the average pp over hauls, all species included
df_mean_pp_per_rec<-df_pp |> 
  select(SurveyYear,rec,HaulNumber,CatchSpeciesCode,
         CatchLengthClass, CatchNumberAtLength,pp) |> 
  group_by(SurveyYear,rec,CatchSpeciesCode, 
           CatchLengthClass) |> 
  #summarise(mean_pp=mean(pp)) # THIS PROBABLY IS INCORRECT
  summarise(sum_pp=sum(pp)) |> 
  left_join(df_n_hauls_per_case) |> 
  mutate(mean_pp=sum_pp/n_hauls_per_case) # THIS SHOULD BE MORE CORRECT
df_mean_pp_per_rec  

# then calculate p per length per species per rectangle
df_sum_pp_per_species<-df_mean_pp_per_rec |> 
  summarise(sum_pp_per_species=sum(mean_pp))

df_pp2<-df_mean_pp_per_rec |> 
  left_join(df_sum_pp_per_species) |> 
  mutate(pp2=mean_pp/sum_pp_per_species) 
df_pp2

# Check that the pp2's sum to 1
print(x=df_pp2|>summarise(sum(pp2)), 
      n=100)

# Divide the number per species per rectangle into lengths 
df_n_per_rec<-df_species |>
  select(rec,CatchSpeciesCode,n_per_species_per_rectangle)

df_pp2_per_length<-df_pp2|> ungroup() |> 
  select(rec,CatchSpeciesCode,pp2,CatchLengthClass)

df_n_per_length<-left_join(df_pp2_per_length,df_n_per_rec) |> 
  mutate(n_per_length=pp2*n_per_species_per_rectangle) |> 
  select(-n_per_species_per_rectangle, -pp2) |> 
  select(year, everything()) 

pivot_n_per_length<-df_n_per_length|> 
  pivot_wider(names_from = CatchSpeciesCode, values_from = n_per_length) |> 
  arrange(rec,CatchLengthClass)
write_xlsx(pivot_n_per_length, "../pivot_n_per_length.xlsx")
# View(pivot_n_per_length)

# ================================
# Age at length
# ================================

# The SD and rectangle link
df_rec_ICES_SD<-df_species |> ungroup() |> select(rec, ICES_SD) |> distinct() |> 
  mutate(ICES_SD=ifelse(rec=="43H2"|
                          rec=="43H3"|
                          rec=="43H4"|
                          rec=="44H2"|
                          rec=="44H3"|
                          rec=="44H4"|
                          rec=="45H2"|
                          rec=="45H3"|
                          rec=="45H4", 28.1, ICES_SD)) 
df_rec_ICES_SD

df_bio_SD<-df_bio |> #bio_all |>
  select(HaulNumber,CatchSpeciesCode,BiologyIndividualAge,BiologyLengthClass) |> 
  left_join(df_hauls_rec) |> 
  select(SurveyYear, CatchSpeciesCode,rec,HaulNumber, everything()) |> 
left_join(df_rec_ICES_SD)
#View(df_bio_SD)

n_per_age_length<-df_bio_SD |> 
  group_by(SurveyYear, CatchSpeciesCode, BiologyIndividualAge,BiologyLengthClass, ICES_SD) |> 
  summarise(n=n())|> 
  select(SurveyYear,ICES_SD, CatchSpeciesCode,  everything()) |> 
  arrange(SurveyYear, ICES_SD)
#View(n_per_age_length)
  
# Sum the number of individuals per length class 
df_sum_per_length_class<-n_per_age_length |> ungroup() |> 
  group_by(SurveyYear, CatchSpeciesCode,ICES_SD,BiologyLengthClass) |> 
  summarise(sum_per_length_class=sum(n)) |> 
  select(SurveyYear,ICES_SD,CatchSpeciesCode, everything())
#View(df_sum_per_length_class)

# Calculate the percentage at age per length class
df_p_age_at_length<-n_per_age_length |> full_join(df_sum_per_length_class) |> 
  mutate(p_age_at_length=n/sum_per_length_class)
#View(df_p_age_at_length)

pivot_p_age_at_length<-df_p_age_at_length|> 
  select(-n, -sum_per_length_class) |> 
  pivot_wider(names_from = BiologyIndividualAge, values_from = p_age_at_length) |> 
  arrange(CatchSpeciesCode,ICES_SD,BiologyLengthClass) |> 
  select(SurveyYear, ICES_SD,CatchSpeciesCode, BiologyLengthClass,`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`, `8`,`9`,`10`,`11`,`12`,everything()) 
pivot_p_age_at_length

#write_xlsx(pivot_p_age_at_length, "../pivot_p_age_at_length.xlsx")

# Abundance at age for herring and sprat
# ================================

# The age length key is SD based
age_length_key<-df_p_age_at_length |>
  mutate(BiologyIndividualAge=as.numeric(BiologyIndividualAge)) |> 
  ungroup() |> 
  select(CatchSpeciesCode,ICES_SD, BiologyLengthClass, BiologyIndividualAge, p_age_at_length)

df_n_per_length_ICES_SD<-df_n_per_length |> 
  left_join(df_rec_ICES_SD)|> 
  select(CatchSpeciesCode,ICES_SD,rec,CatchLengthClass, n_per_length)

df_n_at_age<-df_n_per_length_ICES_SD |>
  mutate(BiologyLengthClass=CatchLengthClass) |> 
  left_join(age_length_key, relationship="many-to-many") |> 
  mutate(n_age_at_length=p_age_at_length*n_per_length)
print(x=df_n_at_age, n=100)

pivot_n_at_age<-df_n_at_age |> group_by(CatchSpeciesCode, 
  rec, BiologyIndividualAge) |> 
  summarise(n_at_age= round(sum(n_age_at_length),2)) |> 
  pivot_wider(names_from = BiologyIndividualAge, values_from = n_at_age) |> 
  mutate(Ntot=sum(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`, na.rm = T)) |> 
  select(CatchSpeciesCode,rec,Ntot,`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,everything())
print(x=pivot_n_at_age, n=100)

# Number at length for other species than herring & sprat
df_n_other<-df_n_per_length|> filter(CatchSpeciesCode!=126417 & CatchSpeciesCode!=126425) |> 
  ungroup() |> group_by(rec, CatchLengthClass) |>
  summarise(n=sum(n_per_length, na.rm = T))

df_n_other_tot<-df_n_other |> 
  summarise(Ntot=sum(n))

pivot_n_other<-df_n_other |> arrange(CatchLengthClass) |> 
  pivot_wider(names_from=CatchLengthClass, values_from=n) |> 
  left_join(df_n_other_tot) |> 
  select(rec, Ntot, everything())


# Weight/Biomass
# ================================


# Mean weight at length per species per haul
df_mean_w_at_length_per_haul<-df_catch |> 
  #IS CatchWeightAtLength in kg's? 
  mutate(mean_w_at_length_per_haul=CatchWeightAtLength*1000/CatchNumberAtLength) |> # OK
  full_join(df_hauls_rec) |> 
  select(rec,HaulNumber,CatchSpeciesCode, CatchLengthClass, mean_w_at_length_per_haul) 

# Mean weight per rec (equal weights on hauls) per length per species
df_mean_w_at_length_per_rec<-df_mean_w_at_length_per_haul |> 
  group_by(rec,CatchSpeciesCode, CatchLengthClass) |> 
  summarise(sum_w=sum(mean_w_at_length_per_haul, na.rm=T)) |> 
  left_join(df_n_hauls_per_case) |> 
  mutate(mean_w_at_length=sum_w/n_hauls_per_case) #OK

# Biomass per length per rectangle = n per length per rec * mean w_at length
df_bm_at_length<-df_n_per_length |> 
  left_join(df_mean_w_at_length_per_rec, relationship="many-to-many") |> 
  mutate(bm_per_length=mean_w_at_length*n_per_length)
print(x=df_bm_at_length, n=1000)

df_bm_per_length_ICES_SD<-df_bm_at_length |> 
  left_join(df_rec_ICES_SD)|> 
  select(CatchSpeciesCode,ICES_SD,rec,CatchLengthClass, bm_per_length)

df_bm_at_age<-df_bm_per_length_ICES_SD |>
  mutate(BiologyLengthClass=CatchLengthClass) |> 
  left_join(age_length_key, relationship="many-to-many") |> 
  mutate(bm_age_at_length=p_age_at_length*bm_per_length)
print(x=df_bm_at_age, n=100)

pivot_bm_at_age<-df_bm_at_age |> 
  group_by(CatchSpeciesCode, rec, BiologyIndividualAge) |> 
  summarise(bm_at_age= round(sum(bm_age_at_length, na.rm=T),2)) |> 
  #arrange(BiologyIndividualAge) |> 
  pivot_wider(names_from = rec, values_from = bm_at_age) |> 
  mutate(Ntot=sum(`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`, na.rm = T)) |> 
  select(CatchSpeciesCode,rec,Ntot,#`0`,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,
         everything())
print(x=pivot_bm_at_age, n=100)

  


# ==========================
# RESULT FILE
# ==========================
# Writes a data frame to an xlsx file. To create an xlsx with (multiple) named sheets, 
# simply set x to a named list of data frames.
AH<-pivot_n_at_age|> filter(CatchSpeciesCode==126417)
AS<-pivot_n_at_age|> filter(CatchSpeciesCode==126425)

res<-list(AH=AH, AS=AS, AO=pivot_n_other)

write_xlsx(res,"../../01-Projects/AUTOFISH/out/EST_BIAS_2024_new.xlsx")


#Other species on an extra sheet on EST_BIAS...xlsx
# Abundance per length group per rectangle
# mean weight per length per rectangle

CatchNumberAtLength
CatchWeightAtLength







