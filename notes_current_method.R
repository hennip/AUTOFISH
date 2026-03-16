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


dfA
catch_all
hauls_all
bio_all

choose_year<-2024

df_acou<-dfA |> filter(year==choose_year)
df_catch<-catch_all|> filter(SurveyYear==choose_year) |> 
  mutate(CatchSpeciesCategoryNumber=as.numeric(CatchSpeciesCategoryNumber))
df_hauls<-hauls_all|> filter(SurveyYear==choose_year)
df_bio<-bio_all|> filter(SurveyYear==choose_year)

# First define the ICES rectangles and then sum over NASC from different depth layers
df<-df_acou |> mutate(rec=ices.rect2(LogLongitude, LogLatitude)) |> 
  select(rec, everything()) |> 
    group_by(year, rec, LogDistance) |> 
    summarise(edsu=sum(DataValue)) # elementary distance sampling unit

# Take mean NASC per ICES rectangle
df2<-df |> group_by(year, rec) |> 
  summarise(mean_edsu=mean(edsu))
  
  
View(df)

View(df_hauls)
View(df_catch)

df_haul_rec<-df_hauls |> select(SurveyYear, HaulNumber, HaulStatisticalRectangle)

# persentage of species in the haul

df1<-df_catch |> left_join(df_haul_rec) |> select(HaulStatisticalRectangle, everything()) |> 
  select(CatchSpeciesCategoryNumber, CatchSpeciesCode, everything())

df2<-df1 |> 
  select(CatchSpeciesCategoryNumber, CatchSpeciesCode, SurveyYear, HaulNumber,HaulStatisticalRectangle) |> 
  distinct() 

tot_catch_per_haul<-df2|> 
  group_by(SurveyYear, HaulNumber, HaulStatisticalRectangle) |> 
  summarise(tot_catch=sum(CatchSpeciesCategoryNumber))

p_species_per_rectangle<-df2  |> left_join(tot_catch_per_haul) |> mutate(p_haul=CatchSpeciesCategoryNumber/tot_catch*100) |> 
  group_by(SurveyYear, CatchSpeciesCode,HaulStatisticalRectangle) |> 
  summarise(p_rec=mean(p_haul)) # NOTE! THIS GIVES EQUAL WEIGHTS FOR HAULS OF DIFFERENT SIZE!!!
print(x=p_species_per_rectangle, n=50)


df2

View(df)
  #select(CatchSpeciesCategoryNumber, HaulNumber, , everything())


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
