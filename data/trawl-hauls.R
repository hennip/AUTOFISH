###############
# Haul data
###############

dfB_haul<-hauls_all %>% mutate(year=SurveyYear)

# How many years in the data?
tmp<-dfB_haul |> group_by(year) |> summarise(n=n())
Nyears<-length(tmp$year)
min_years<-min(tmp$year)

# Define rec_ruhnu 
# 1: NW from ruhnu 
# 2: NE from ruhnu 
# 3: SW from ruhnu 
# 4: SE from ruhnu 
dfB_haul2<-dfB_haul|> 
  mutate(
    HaulNumber=as.numeric(HaulNumber),
    latStart=as.numeric(HaulStartLatitude),
    longStart=as.numeric(HaulStartLongitude),
    latStop=as.numeric(HaulStopLatitude),
    longStop=as.numeric(HaulStopLongitude)) |> 
  mutate(rec_ruhnu=ifelse(latStart>=ruhnuLat & longStart<ruhnuLong, 1,NA)) |>         # 1: NW
  mutate(rec_ruhnu=ifelse(latStart>=ruhnuLat & longStart>=ruhnuLong, 2,rec_ruhnu)) |> # 2: NE 
  mutate(rec_ruhnu=ifelse(latStart<ruhnuLat & longStart<ruhnuLong, 3,rec_ruhnu)) |>   # 3: SW 
  mutate(rec_ruhnu=ifelse(latStart<ruhnuLat & longStart>=ruhnuLong, 4,rec_ruhnu)) |>  # 4: SE 
  mutate(minDepth=as.numeric(HaulMinTrawlDepth), maxDepth=as.numeric(HaulMaxTrawlDepth)) |> 
  select( year, rec_ruhnu, latStart, longStart, minDepth, maxDepth, HaulNumber, everything()) |> 
  mutate(rec=as.factor(rec_ruhnu))
dfB_haul2
#View(dfB_haul2)

df_rec<-dfB_haul2 |> select(year,rec_ruhnu, HaulNumber)
#View(df_rec)

# Number of hauls per rectangle: Nhaul[r,y]
Nhaul<-
  as.matrix(dfB_haul2 |> group_by(year, rec_ruhnu) |> summarise(n=n()) |> 
              pivot_wider(names_from = year, values_from = n) |> 
              ungroup() |> select(-rec_ruhnu))


minmax_depth<-dfB_haul2 |> arrange(rec) |> group_by(year,rec) |> 
  mutate(rec=as.numeric(rec)) |> 
  summarise(min_trawl_depth=min(minDepth), max_trawl_depth=max(maxDepth))#, mean=(min+max)/2)


# df<-dfB_haul |> select(rec, minDepth, maxDepth, HaulNumber) |> 
#   pivot_longer(cols = minDepth:maxDepth, names_to="minmax",values_to = "depth")
# 
# ggplot(data=df, aes(x=HaulNumber, y=depth, group=rec))+
#   geom_line(aes(col=rec))

#View(dfB_haul)
