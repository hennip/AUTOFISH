# 

# Read in data from all countries
source("02-current-method/read-in-data-BIAS-all-countries.R")

# Define the year to be investigated
choose_year<-2025

##################################
# Modify datasets: 
# Filter year and transform chr variables to numeric where needed
# Join rectangle and ICES_SD to data tables
##################################

df_acou<-dfA |> filter(SurveyYear==choose_year) |> 
  mutate(DataValue=as.numeric(DataValue),
    LogLatitude=as.numeric(LogLatitude), 
    LogLongitude=as.numeric(LogLongitude),
    year=SurveyYear)
         
df_hauls<-hauls_all|> 
  filter(SurveyYear==choose_year)|> 
  mutate(rec=HaulStatisticalRectangle) |> select(-HaulStatisticalRectangle) |> 
  select(SurveyYear,country,HaulNumber,rec)

# Number of hauls per rectangle
df_n_hauls_per_rec<-df_hauls_rec |> group_by(rec) |> 
  summarise(n_hauls_per_rec=n())

df_catch<-catch_all|> filter(SurveyYear==choose_year) |> 
  mutate(CatchSpeciesCategoryNumber=as.numeric(CatchSpeciesCategoryNumber),
         CatchNumberAtLength=as.numeric(CatchNumberAtLength),
         CatchLengthClass=as.numeric(CatchLengthClass),
         CatchWeightAtLength=as.numeric(CatchWeightAtLength),
         CatchNumberAtLength=as.numeric(CatchNumberAtLength),
         species=CatchSpeciesCode) |> 
  left_join(df_hauls) |> # link rectangle with haul number 
  select(rec,species,everything()) |> 
  left_join(df_rec_info, relationship="many-to-many") # link SD and area info to the rec

df_biol<-biol_all|> filter(SurveyYear==choose_year) |> 
  left_join(df_hauls) |> # link rectangle with haul number
  mutate(BiologyLengthClass =as.numeric(BiologyLengthClass ),
         species=CatchSpeciesCode,
         age=BiologyIndividualAge) |> 
  left_join(df_rec_info, relationship="many-to-many") # link SD and area info to the rec


# ==============================================================================
# BASIC DATA WRANGLING
# ==============================================================================
# ices.rect2-function example:
# lon <- c(21.97854644, 23.03531506)
# lat <- c(57.76010658, 57.83974327)
# # Convert to ICES rectangles
# rect <- ices.rect2(lon, lat); rect

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
#View(df_nasc)

# Catch sample sizes per length per haul
df_sample_size_per_length<-df_catch_w_rec |> 
  group_by(species, HaulNumber) |> 
  summarise(sample_size_per_length=sum(CatchNumberAtLength))

# Join sample sizes per length with the catch table 
# and calculate the percentage of individuals in a specific length class
df_p_per_length<-df_catch_w_rec |> 
  left_join(df_sample_size_per_length) |> 
  mutate(p_per_length=CatchNumberAtLength/sample_size_per_length*100) |> 
  select(rec, HaulNumber, species,CatchLengthClass, p_per_length, everything()) 
df_p_per_length
#View(df_p_per_length)
#write_xlsx(df_p_per_length, "../p_per_length.xlsx")

# THIS JUST INTERESTING TO SEE?
tmp<-df_p_per_length |> 
  group_by(rec,HaulNumber, species) |> 
  summarise(sum=sum(p_per_length))
#View(tmp)

# Number of hauls per rectangle and per species
df_n_hauls_per_case<- df_p_per_length|>
  group_by(rec,species) |>
  summarise(n_hauls_per_case=n_distinct(HaulNumber))
#print(x=df_n_hauls_per_case, n=100)

# Rectangle specific proportion of individuals of certain length is the
# mean over length class specific percentages
df_p_per_length_per_rec<-df_p_per_length |>
  group_by(rec, species, CatchLengthClass) |>
  summarise(sum_p_per_length_per_rec=sum(p_per_length)) |>
  left_join(df_n_hauls_per_case) |>
  mutate(mean_p_per_length_per_rec=sum_p_per_length_per_rec/n_hauls_per_case)
#write_xlsx(df_p_per_length_per_rec, "../df_p_per_length_per_rec.xlsx")

# check that all sum to 100. Rounding removes tiny irrelevant differences that sometimes occur
df_p_per_length_per_rec|>
  summarise(sum_p=round(sum(mean_p_per_length_per_rec),5)) |> 
  filter(sum_p!=100)

# Haul specific 
# ==============
# Total catch per haul per species
# NOTE! Because in some occasions the catch is divided into several 
# length categories, we need to sum over those to get the species
# specific catch
df_catch_per_species<-df_catch_w_rec |> 
  select(HaulNumber,rec, species, CatchSpeciesCategoryNumber) |> 
  distinct() |>
  group_by(HaulNumber, rec, species) |> 
  summarise(n_per_species=sum(CatchSpeciesCategoryNumber))

df_catch_per_species |> filter(HaulNumber>50000, species==126417)

# Total catch per haul, all species
df_tot_catch_per_haul<-df_catch_per_species|> 
  group_by(HaulNumber, rec) |> 
  summarise(tot_catch_per_haul=sum(CatchSpeciesCategoryNumber))

# Proportion of each species per haul
df_p_species_per_haul<-df_catch_per_species  |>
  left_join(df_tot_catch_per_haul) |>
  mutate(p_species_per_haul=n_per_species/tot_catch_per_haul)
#print(x=df_p_species_per_haul, n=100)

# Rectangle specific 
# ==============
# Total catch per rectangle, all species
df_tot_catch_per_rec<-df_catch_per_species|> 
  group_by(rec) |> 
  summarise(tot_catch_per_rec=sum(n_per_species))

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
  mutate(p_species=n_per_species/tot_catch_per_haul*100) |>
  group_by(rec, species) |>
  summarise(sum_species_per_rec=sum(p_species)) |> 
  full_join(df_n_hauls_per_rec) |> 
  mutate(p_species_per_rec=sum_species_per_rec/n_hauls_per_rec) # EQUAL WEIGHTS FOR HAULS
  #summarise(p_species_per_rec=mean(p_species)) # Direct mean cannot be taken since often all species are not present in every haul
print(x=df_p_species_per_rectangle, n=100)

# check that all sum to 100. Rounding removes tiny irrelevant differences that sometimes occur
df_p_species_per_rectangle |> 
  summarise(sum=round(sum(p_species_per_rec),5)) |> 
  filter(sum!=100)

# pp is haul specific
# pp is the product of the proportion of a species
# and the percentage of species at certain length 
df_pp<-df_p_per_length |> full_join(df_p_species_per_haul) |> 
  mutate(pp=p_per_length*p_species_per_haul) |> 
  select(pp, everything())
df_pp

# Check that pp's sum to 100. Rounding removes tiny irrelevant differences that sometimes occur
df_pp |> group_by(HaulNumber) |> summarise(sumx=round(sum(pp),5)) |> 
  filter(sumx!=100)

# ================================
# SIGMA CALCULATIONS
# ================================

# For each row in the df_pp, calculate sigma
df_sigma<-df_pp |> 
  mutate(d=9.5325669476e-07) |> # d is the same for all clupeids
  #  mutate(d=ifelse(species==xxxx, 9.1, d) # Example on how to change d for a specific species xxxx
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
  select(species, rec, p_species_per_rec) |> 
  pivot_wider(names_from = species, values_from = p_species_per_rec) 
pivot_p_per_species

# Join percentage of species per rectangle (unpivoted) with the other
# rectangle specific data
# Calculate number of individuals per species per NM2, 
# total number of individuals and 
# the number of individuals per species per rectangle  
# Note! many-to-many warning is ok when joining since proportion of 
# each species must be linked with the same rectangle data 
df_species<-full_join(df_nasc_per_rectangle, df_p_species_per_rectangle, relationship="many-to-many") |> 
  mutate(N_per_NM2=mean_nasc/sigma_rectangle/1000000,
         Ntot=N_per_NM2*A_NM2) |> 
  select(year, rec, N_per_NM2, Ntot, everything()) |> 
  mutate(n_per_species_per_rectangle=p_species_per_rec/100*Ntot)
df_species
#View(df_species)

# Pivot: Number of individuals per species per rectangle
df_species |>   
  select(-p_species_per_rec) |> 
  pivot_wider(names_from = species, values_from = n_per_species_per_rectangle)

# ================================
# LENGTH SPECIFIC ABUNDANCES
# ================================

# Following the excel way of doing, assume all hauls have equal weights
# regardless of the sample size 

# take first the average pp over hauls, all species included
df_mean_pp_per_rec<-df_pp |> 
  select(SurveyYear,rec,HaulNumber,species,
         CatchLengthClass, CatchNumberAtLength,pp) |> 
  group_by(SurveyYear,rec,species, 
           CatchLengthClass) |> 
  #summarise(mean_pp=mean(pp)) # THIS PROBABLY IS INCORRECT
  summarise(sum_pp=sum(pp)) |> 
  left_join(df_n_hauls_per_case) |> 
  mutate(mean_pp=sum_pp/n_hauls_per_case) # THIS SHOULD BE CORRECT
df_mean_pp_per_rec  

# then calculate p per length per species per rectangle
df_sum_pp_per_species<-df_mean_pp_per_rec |> 
  summarise(sum_pp_per_species=sum(mean_pp))

df_pp2<-df_mean_pp_per_rec |> 
  left_join(df_sum_pp_per_species) |> 
  mutate(pp2=mean_pp/sum_pp_per_species) 
df_pp2

# Check that the pp2's sum to 1
df_pp2|>summarise(sum=round(sum(pp2),5)) |> filter(sum!=1)
      
# Divide the number per species per rectangle into lengths 
df_n_per_rec<-df_species |>
  select(rec,species,n_per_species_per_rectangle)

df_pp2_per_length<-df_pp2|> ungroup() |> 
  select(rec,species,pp2,CatchLengthClass)

df_pp2_per_length |> filter(rec=="39G2", CatchLengthClass==105)

# SUBDIVISIONS NEED TO BE INCLUDED EARLIER!!!!
df_n_per_rec |> filter(rec=="39G2", species==126417)
# Join proportions per length group with number of species per rec
# many-to-many is ok since each length group is linked with the 
# total number of that species
df_n_per_length<-left_join(df_pp2_per_length,df_n_per_rec, 
                           relationship="many-to-many") |> 
  mutate(n_per_length=round(pp2*n_per_species_per_rectangle,3)) |> 
  select(-n_per_species_per_rectangle, -pp2) |> 
  select(year, everything()) 

df_n_per_length |> filter(rec=="39G2", CatchLengthClass==105)

# ================================
# Age at length
# ================================

# Link rectangles to correct ICES SD 
# Define SD 28.1
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

# Join ICES SD info to the biol table.
# Many-to-many is ok as the join just links the SD info 
df_bio_SD<-df_bio |>
  select(HaulNumber,species,age,BiologyLengthClass) |> 
  left_join(df_hauls_rec) |> 
  select(species,rec,HaulNumber, everything()) |> 
left_join(df_rec_ICES_SD, relationship = "many-to-many")

n_per_age_length<-df_bio_SD |> 
  group_by(species, age,BiologyLengthClass, ICES_SD) |> 
  summarise(n=n())|> 
  select(ICES_SD, species,  everything()) |> 
  arrange(ICES_SD)
#View(n_per_age_length)
  
# Sum the number of individuals per length class 
df_sum_per_length_class<-n_per_age_length |> ungroup() |> 
  group_by(species,ICES_SD,BiologyLengthClass) |> 
  summarise(sum_per_length_class=sum(n)) |> 
  select(ICES_SD,species, everything())

# Calculate the percentage at age per length class
df_p_age_at_length<-n_per_age_length |> full_join(df_sum_per_length_class) |> 
  mutate(p_age_at_length=n/sum_per_length_class)

pivot_p_age_at_length<-df_p_age_at_length|> 
  select(-n, -sum_per_length_class) |> 
  arrange(species,ICES_SD,BiologyLengthClass,age) |> 
  pivot_wider(names_from = age, values_from = p_age_at_length) #|> 
pivot_p_age_at_length


# Abundance at age for herring and sprat
# ================================

# The age length key is SD based
age_length_key<-df_p_age_at_length |>
  mutate(age=as.numeric(age)) |> 
  ungroup() |> 
  select(species,ICES_SD, BiologyLengthClass, age, p_age_at_length)

#age_length_key |> distinct()

# Join SD's, many-to-many is ok
df_n_per_length_ICES_SD<-df_n_per_length |> 
  left_join(df_rec_ICES_SD, relationship="many-to-many")|> 
  select(species,ICES_SD,rec,CatchLengthClass, n_per_length)

df_n_per_length_ICES_SD |> filter(rec=="39G2", ICES_SD==24, CatchLengthClass==105)

# Join age length key with numbers at length. 
# Many-to-many is ok
df_n_at_age<-df_n_per_length_ICES_SD |>
  mutate(BiologyLengthClass=CatchLengthClass) |> 
  left_join(age_length_key, relationship="many-to-many") |> 
  mutate(n_age_at_length=p_age_at_length*n_per_length)
print(x=df_n_at_age, n=100)

df_n_at_age |> filter(rec=="39G2", ICES_SD==24, age==0, CatchLengthClass==105)

pivot_n_at_age<-df_n_at_age |> group_by(species, rec, age) |> 
  summarise(n_at_age= round(sum(n_age_at_length),2)) |> 
  left_join(df_rec_ICES_SD, relationship="many-to-many") |> 
  arrange(age, species, ICES_SD,rec) |> 
  pivot_wider(names_from = age, values_from = n_at_age) |> 
  mutate(NTOT=rowSums(across(c(`0`:`11`)), na.rm = T)) |> 
  rename(N0=`0`,N1=`1`,N2=`2`,N3=`3`,N4=`4`,N5=`5`,N6=`6`,N7=`7`,N8=`8`,
         N9=`9`,N10=`10`,N11=`11`#,N12=`12`
         )|> 
  select(species,ICES_SD,rec,NTOT,everything()) |> 
  ungroup()
print(x=pivot_n_at_age, n=100)

# Other species per rec and length
df_n_per_length2<-df_n_per_length |>
  left_join(df_rec_ICES_SD, relationship="many-to-many") |> 
  group_by(rec, species) |> 
  arrange(CatchLengthClass,species, ICES_SD,rec)  

#df_n_per_length2
#View(df_n_per_length2 |> filter(n_per_length>1000))
# SOME CRAZY LARGE NUMBERS IN 38G1
  
pivot_n_per_length<-df_n_per_length2 |>   pivot_wider(names_from=CatchLengthClass, values_from = n_per_length) |> 
  select(-year) |> select(species, ICES_SD,rec, everything())
#View(pivot_n_per_length)
# NOTE!!! Several values for the same length group! 
# Is this because the same name is assigned to different recs
# or what is going on?

# Weight/Biomass
# ================================

# Mean weight at length per species per haul if given at the catch table
df_mean_w_at_length_per_haul_catch<-df_catch |> 
  #IS CatchWeightAtLength in kg's? 
  mutate(mean_w_at_length_per_haul=CatchWeightAtLength*1000/CatchNumberAtLength) |> # OK
  full_join(df_hauls_rec) |> 
  select(rec,HaulNumber,species, CatchLengthClass, mean_w_at_length_per_haul) |> 
  filter(is.na(mean_w_at_length_per_haul)==F)

# Mean weight at length per species per haul if individual weights given 
# at the biology table
df_mean_w_at_length_per_haul_biol<-df_bio |> 
  filter(is.na(BiologyIndividualWeight)==F) |> 
  select(rec, HaulNumber, species, BiologyLengthClass, BiologyIndividualWeight) |> 
 distinct() |> 
    mutate(mean_w_at_length_per_haul=as.numeric(BiologyIndividualWeight)) |> 
  rename(CatchLengthClass=BiologyLengthClass) |> 
  select(-BiologyIndividualWeight)
  #group_by(HaulNumber, BiologyLengthClass) |> 
   
# Combine
df_mean_w_at_length_per_haul<-full_join(df_mean_w_at_length_per_haul_catch,df_mean_w_at_length_per_haul_biol)
#View(df_mean_w_at_length_per_haul)

# Mean weight per rec (equal weights on hauls) per length per species
df_mean_w_at_length_per_rec<-df_mean_w_at_length_per_haul |> 
  group_by(rec,species, CatchLengthClass) |> 
  summarise(sum_w=sum(mean_w_at_length_per_haul, na.rm=T)) |> 
  left_join(df_n_hauls_per_case) |> 
  mutate(mean_w_at_length=round(sum_w/n_hauls_per_case,2)) #OK

pivot_mean_weight_per_length<-df_mean_w_at_length_per_rec |> 
  select(rec, species,CatchLengthClass, mean_w_at_length) |> 
  group_by(rec, species, CatchLengthClass) |> 
  arrange(CatchLengthClass, species, rec) |> 
  pivot_wider(values_from = mean_w_at_length, names_from = CatchLengthClass)

# Biomass per length per rectangle = n per length per rec * mean w_at length
# Unit is grams times millions individuals = millions of grams = tonnes
df_bm_at_length<-df_n_per_length |> 
  left_join(df_mean_w_at_length_per_rec, relationship="many-to-many") |> 
  mutate(bm_per_length=mean_w_at_length*n_per_length)
print(x=df_bm_at_length, n=1000)

df_bm_per_length_ICES_SD<-df_bm_at_length |> 
  left_join(df_rec_ICES_SD, relationship="many-to-many")|> 
  select(species,ICES_SD,rec,CatchLengthClass, bm_per_length)

# Biomass per age for herring and sprat
df_bm_at_age<-df_bm_per_length_ICES_SD |>
  mutate(BiologyLengthClass=CatchLengthClass) |> 
  left_join(age_length_key, relationship="many-to-many") |> 
  mutate(bm_age_at_length=p_age_at_length*bm_per_length)
print(x=df_bm_at_age, n=100)

pivot_bm_at_age<-df_bm_at_age |> 
  filter(species==126417 | species==126425) |> 
  group_by(species, rec, age) |> 
  summarise(bm_at_age= round(sum(bm_age_at_length, na.rm=T),2)) |> 
  left_join(df_rec_ICES_SD)|> 
  arrange(age,species,ICES_SD) |> 
  pivot_wider(names_from = age, values_from = bm_at_age) |> 
  mutate(WTOT=rowSums(across(c(`0`:`11`)), na.rm = T)) |>
  rename(W0=`0`,W1=`1`,W2=`2`,W3=`3`,W4=`4`,W5=`5`,W6=`6`,W7=`7`,W8=`8`,
         W9=`9`,W10=`10`,W11=`11`#,W12=`12`
         )|> 
  select(species,ICES_SD, rec, WTOT, everything()) |> 
  ungroup()
print(x=pivot_bm_at_age, n=100)

# Mean weight at age
# join bm at age
# NOTE! FOR SOME REASON THERE ARE DUPLICATE ROWS!!! SOMETHING WRONG WITH THE SCRIPT?
df_mean_weight_at_age<-df_bm_at_age|>#distinct()
  full_join(df_n_at_age) #|> 
  summarise(mean_weight_at_age=
              round(sum(bm_age_at_length, na.rm=T)/
                      sum(n_age_at_length, na.rm=T),2))

View(df_bm_at_age)
df_bm_at_age |> filter(is.na(age)==T)
View(df_n_at_age)  
df_n_at_age |> filter(is.na(age)==T)

df_mean_weight_at_age<-df_bm_at_age|>
  select(species, ICES_SD, rec, CatchLengthClass, bm_per_length, age) |> 
   full_join(df_n_at_age |> 
               select(species, ICES_SD, rec, CatchLengthClass, age, n_age_at_length)) |> 
#  full_join(df_n_at_age) |> 
  summarise(mean_weight_at_age=
              round(sum(bm_age_at_length, na.rm=T)/
                      sum(n_age_at_length, na.rm=T),2))

pivot_mean_weight_at_age <-df_mean_weight_at_age |> 
  pivot_wider(names_from = age, values_from = mean_weight_at_age) |> 
  left_join(df_rec_ICES_SD) |> select(species, ICES_SD, rec, everything()) 
  
# Biomass per length for other species than herring & sprat
pivot_bm_per_length<-df_bm_per_length_ICES_SD  |> 
  group_by(rec, species) |> 
  arrange(CatchLengthClass,species, rec) |> 
  pivot_wider(names_from=CatchLengthClass, values_from = bm_per_length)|> 
  #mutate(WTOT=round(rowSums(across(c(`9`:`175`)), na.rm = T),2)) |> 
  select(species, ICES_SD, rec, #WTOT, 
         everything())
pivot_bm_per_length

# ==========================
# RESULT FILE
# ==========================
AH<-pivot_n_at_age|> filter(species==126417) |> select( -`NA`)
AS<-pivot_n_at_age|> filter(species==126425)|> select( -`NA`)
AO<-pivot_n_per_length|> filter(species!=126417 & species!=126425)

# Biomass per species if of interest
#pivot_bm_at_age|> filter(species==126417)
#pivot_bm_at_age|> filter(species==126425)
#pivot_bm_per_length|>filter(species!=126417 & species!=126425)

WH<-pivot_mean_weight_at_age|> filter(species==126417)|> select( -`NA`)
WS<-pivot_mean_weight_at_age|> filter(species==126425)|> select( -`NA`)
WO<-pivot_mean_weight_per_length


#ST sheet
df_sigma_rectangle |> 
  left_join(df_rec_ICES_SD)|> 
  mutate(SIGMA=round(sigma_rectangle*10000,digits=3),
         YEAR=choose_year) |> 
  select(-sigma_rectangle) |> 
  left_join(df_nasc|> select(-year, -ICES_SD)) |> 
  rename(RECT=rec, SD=ICES_SD, SA=mean_nasc) |> 
  select(SD, RECT, A_NM2, SA, SIGMA)
  

# To create an xlsx with (multiple) named sheets, 
# simply set x to a named list of data frames.
res<-list(AH=AH, WH=WH, AS=AS, WS=WS, AO=AO, WO=WO)

write_xlsx(res,paste0(path_output, "BIAS_results_", choose_year, ".xlsx"))
#           "../out/EST_GRAHS_2024_new.xlsx")
#write_xlsx(res,"../../01-Projects/AUTOFISH/out/EST_BIAS_2025_new.xlsx")

