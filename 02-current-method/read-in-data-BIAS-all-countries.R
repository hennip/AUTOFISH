rm(list = ls())

source("00-basics/packages-and-paths.R")

#source("data/read-in-acoustic-data.R") 

# # BIAS survey data for 2024
# # NOTE!!! Define path_BIAS in 00-basics/packages-and-paths.R !!!
# dfA24<-read.csv(str_c(path_BIAS,"Acoustic_ESTBIAS2024_2025-01-03T08.14.20.660.csv"), skip=11) |> 
#   as_tibble() |> mutate(year=2024)
source("01-data/func-read-in-acoustic-data.R")

dfA_EE<-read_in_acoustic_data(paste0(pathA,"EE/"))|> mutate(country="EE", CountryCoef=10000)
dfA_FI<-read_in_acoustic_data(paste0(pathA,"FI/"))|> mutate(country="FI", CountryCoef=20000)
dfA_DE<-read_in_acoustic_data(paste0(pathA,"DE/"))|> mutate(country="DE", CountryCoef=30000)
dfA_PL<-read_in_acoustic_data(paste0(pathA,"PL/"))|> mutate(country="PL", CountryCoef=40000)
dfA_SE<-read_in_acoustic_data(paste0(pathA,"SE/"))|> mutate(country="SE", CountryCoef=50000)
dfA_LV<-read_in_acoustic_data(paste0(pathA,"LV/"))|> mutate(country="LV", CountryCoef=60000)
#dfA_LT<-read_in_acoustic_data(paste0(pathA,"LT/"))|> mutate(country="LT", CountryCoef=70000)


dfA <- dfA_EE |>
  full_join(dfA_FI) |>
  full_join(dfA_DE) |>
  full_join(dfA_PL) |>
  full_join(dfA_SE) |>
  full_join(dfA_LV) |>
  select(country, everything()) |>
  mutate(
    LogDistance = as.numeric(LogDistance),
    lognew = LogDistance + CountryCoef
  )

#view(dfA)

source("01-data/func-read-in-trawl-data.R") 

#trawl<-read_in_trawl_data(pathB)
dfB_EE<-read_in_trawl_data(paste0(pathB,"EE/"))
hauls_EE<-dfB_EE[[1]] |> mutate(country="EE", CountryCoef=10000)
catch_EE<-dfB_EE[[2]]|> mutate(country="EE", CountryCoef=10000)
bio_EE<-dfB_EE[[3]]|> mutate(country="EE", CountryCoef=10000)

dfB_FI<-read_in_trawl_data(paste0(pathB,"FI/"))
hauls_FI<-dfB_FI[[1]]|> mutate(country="FI", CountryCoef=20000)
catch_FI<-dfB_FI[[2]]|> mutate(country="FI", CountryCoef=20000)
bio_FI<-dfB_FI[[3]]|> mutate(country="FI", CountryCoef=20000)

dfB_DE<-read_in_trawl_data(paste0(pathB,"DE/"))
hauls_DE<-dfB_DE[[1]]|> mutate(country="DE", CountryCoef=30000)
catch_DE<-dfB_DE[[2]]|> mutate(country="DE", CountryCoef=30000)
bio_DE<-dfB_DE[[3]]|> mutate(country="DE", CountryCoef=30000)

dfB_PL<-read_in_trawl_data(paste0(pathB,"PL/"))
hauls_PL<-dfB_PL[[1]]|> mutate(country="PL", CountryCoef=40000)
catch_PL<-dfB_PL[[2]]|> mutate(country="PL", CountryCoef=40000)
bio_PL<-dfB_PL[[3]]|> mutate(country="PL", CountryCoef=40000)

# if mean weight exists in CatchWeightAtLength column, remove individual weights
# from the biology table
if(dim(catch_PL |> filter(is.na(CatchWeightAtLength)==T))[1]==0){
  bio_PL<-bio_PL |> mutate(BiologyIndividualWeight=NA) |> 
    select(BiologyIndividualWeight,#BiologyIndividualWeight2, 
           everything())
}
#View(bio_PL)

dfB_SE<-read_in_trawl_data(paste0(pathB,"SE/"))
hauls_SE<-dfB_SE[[1]]|> mutate(country="SE", CountryCoef=50000)
catch_SE<-dfB_SE[[2]]|> mutate(country="SE", CountryCoef=50000)
bio_SE<-dfB_SE[[3]]|> mutate(country="SE", CountryCoef=50000)

dfB_LV<-read_in_trawl_data(paste0(pathB,"LV/"))
hauls_LV<-dfB_LV[[1]]|> mutate(country="LV", CountryCoef=60000)
catch_LV<-dfB_LV[[2]]|> mutate(country="LV", CountryCoef=60000)
bio_LV<-dfB_LV[[3]]|> mutate(country="LV", CountryCoef=60000)

if(dim(catch_LV |> filter(is.na(CatchWeightAtLength)==T))[1]==0){
  bio_LV<-bio_LV |> mutate(BiologyIndividualWeight=NA) |> 
    select(BiologyIndividualWeight,#BiologyIndividualWeight2, 
           everything())
}


#dfB_LT<-read_in_trawl_data(paste0(pathB,"LT/"))
#hauls_LT<-dfB_LT[[1]]|> mutate(country="LT", CountryCoef=70000)
#catch_LT<-dfB_LT[[2]]|> mutate(country="LT", CountryCoef=70000)
#bio_LT<-dfB_LT[[3]]|> mutate(country="LT", CountryCoef=70000)

hauls_all<-full_join(hauls_EE, hauls_FI)|> 
  full_join(hauls_DE) |>
  full_join(hauls_PL) |>
  full_join(hauls_SE) |>
  full_join(hauls_LV) |>
  #full_join(hauls_LT) |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(HaulNumber=HaulNumber+CountryCoef) |> 
  select(country, HaulNumber, everything())

catch_all<-full_join(catch_EE, catch_FI)|> 
  full_join(catch_DE) |>
  full_join(catch_PL) |>
  full_join(catch_SE) |>
  full_join(catch_LV) |>
  #full_join(hauls_LT) |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(HaulNumber=HaulNumber+CountryCoef) |> 
  select(country, HaulNumber, everything()) |> 
  mutate(CatchWeightAtLength=as.numeric(CatchWeightAtLength)) |> 
  mutate(CatchWeightAtLength=ifelse(CatchWeightUnit=="gr", CatchWeightAtLength/1000, CatchWeightAtLength)) |> 
  mutate(CatchLengthClass=as.numeric(CatchLengthClass)) |> 
  mutate(CatchLengthClass_mm=ifelse(CatchLengthCode=="cm", CatchLengthClass*10, CatchLengthClass)) |> 
  mutate(offset=ifelse(CatchLengthCode=="cm", 0.45, 
                       ifelse(CatchLengthCode=="halfcm", 0.2, 0))) 

# Note! In some cases there are several length categories for the same species
catch_all |> group_by(country) |>
  mutate(cat=as.numeric(CatchSpeciesCategory)) |> 
  summarise(min=min(cat),max=max(cat))


biol_all<-full_join(bio_EE, bio_FI)|> 
  full_join(bio_DE) |>
  full_join(bio_PL) |>
  full_join(bio_SE) |>
  full_join(bio_LV) |>
  #full_join(hauls_LT) |>
  mutate(HaulNumber=as.numeric(HaulNumber)) |> 
  mutate(HaulNumber=HaulNumber+CountryCoef) |> 
  select(country, everything()) |> 
  mutate(BiologyLengthClass=as.numeric(BiologyLengthClass)) |> 
  mutate(BiologyLengthClass_mm=ifelse(BiologyLengthCode=="cm", BiologyLengthClass*10, BiologyLengthClass)) 
  


#View(bio_all)

# Rectangle specific info: ICES sub division and area as NM^2
df_rec_info<-read_xlsx(str_c("01-data/ICES_rec_areas.xlsx")) |> 
  rename(ICES_SD=SD, rec=ICES_rectangle, A_NM2=Area_NM2)



# dfA
# catch_all
# hauls_all
# biol_all
# rec_areas

