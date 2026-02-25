source("packages-and-paths.r")

####################################
# Read in the acoustic data from GRAHS (Gulf of Riga acoustic survey)
# and modify it for the needs of model run
####################################

# Instead of statistical rectangles, divide the gulf into 4 areas using coordinates
# of Ruhne island (lighthouse) as a limit point

# Coordinates of Ruhne lighthouse according to Wikipedia
ruhneLat<-57.80135766
ruhneLong<-23.26012233

################################
# Acoustic data

dfA24<-read.csv(str_c(path,"Acoustic_2024-ZR038_2025-03-12T10.25.26.053.csv"), skip=11) |> as_tibble() |> mutate(year=2024)
dfA23<-read.csv(str_c(path,"Acoustic_2023-ZR055_2024-02-05T18.20.41.813.csv"), skip=11) |> as_tibble() |> mutate(year=2023) 
dfA<-full_join(dfA24, dfA23)
#View(dfA)
################################


# Divide area to 4 pieces:
# 1: NW from Ruhne 
# 2: NE from Ruhne 
# 3: SW from Ruhne 
# 4: SE from Ruhne 

# Define rec_ruhne based on coordinates of the lighthouse the locations of the cruise track
# shorten names for depth, pick up variables needed later
dfA2<-dfA |> mutate(rec_ruhne=ifelse(LogLatitude>=ruhneLat & LogLongitude<ruhneLong, 1,NA)) |>   # 1: NW
  mutate(rec_ruhne=ifelse(LogLatitude>=ruhneLat & LogLongitude>=ruhneLong, 2,rec_ruhne)) |>      # 2: NE 
  mutate(rec_ruhne=ifelse(LogLatitude<ruhneLat & LogLongitude<ruhneLong, 3,rec_ruhne)) |>        # 3: SW 
  mutate(rec_ruhne=ifelse(LogLatitude<ruhneLat & LogLongitude>=ruhneLong, 4,rec_ruhne))|>        # 4: SE  
mutate(depthLow=SampleChannelDepthLower, depthUpp=SampleChannelDepthUpper)|>
  select(rec_ruhne, DataValue, depthLow, depthUpp, everything()) |> 
  select(-Data, -Header) |> 
  mutate(rec=rec_ruhne)
dfA2
#View(dfA2)  

tot_nasc_per_log<-dfA2 |> group_by(year, rec, LogDistance) |> 
  # Sum nasc over different depths
  summarise(number_of_layers=n(), sum_nasc=sum(DataValue),
            min_depthUpp=min(depthUpp), 
            max_depthLow=max(depthLow), 
            mean_depth=(min_depthUpp+max_depthLow)/2)|>
  mutate(radius=mean_depth*tan((pi/180)*3.5), # angle of 3.5 degrees
         #circle=pi*radius^2, #m2
         width_m=2*radius,
         width_NM=width_m*(1/1852),
         
         area_m2=width_m*1852, # Area in m^2 per 1NM (=1852 m) of cruise track,
         area_NM2=width_NM*1, # # Area in NM^2 per 1NM of cruise track
         test_m2=1852^2*area_NM2 # USE THIS CORRECTION IF USING M2's rather than NM2's! may(?) influence how smooth computation is, although no practical difference as long as keeping it systematic 
  )|> 
  group_by(year, rec) |> 
  mutate(LOG = row_number())|>  # Add running number for logs per rectangle. Note! grouping by rec and year is mandatory
 select(year, rec, LOG, everything())
tot_nasc_per_log
#View(tot_nasc_per_log)

#Necho[r,y]
Necho_per_rec<-tot_nasc_per_log |> group_by(year, rec) |> summarise(n=n()) |> 
  pivot_wider(names_from = year, values_from = n) |> select(-rec)
Necho_per_rec

necho<-as.matrix(Necho_per_rec)

# nascY is year index
nascY=tot_nasc_per_log |>ungroup() |> select(year) |> mutate(year=year-2022)


# Testing stuff
################################################################################

# psi: indicator of whether the depth is less than 25m
tmp<-dfA2 |> mutate(psi=ifelse(depthLow>25, 1, 0)) |> group_by(LogDistance, psi, rec) |> 
  summarise(nasc_tot=sum(DataValue))
tmp
# About 15% of observations is from deeper layers than 25m
tmp |> group_by(psi) |> summarise(tot=sum(nasc_tot))

# Run minmax_depth from trawl data (code below)
# delta: does the acoustic go lower than the trawl data in corresponding ruhne-rectangle?
tmp2<-full_join(dfA2, minmax_depth) |> 
  select(min_trawl_depth, max_trawl_depth, rec, DataValue, depthUpp, depthLow, LogDistance, LogLatitude, LogLongitude) |> 
  arrange(rec) |> 
  mutate(delta=ifelse(depthLow>max_trawl_depth, 1, 0))

View(tmp2)



# Calculate autocorrelation of nascs as the distance increases (chatGPT)
#######################################################################
obs <- tmp3$sum_nasc
max_lag <- length(obs) - 1
lag_cor <- numeric(max_lag)
for (h in 1:max_lag) {
  lag_cor[h] <- cor(obs[1:(length(obs)-h)], 
                    obs[(1+h):length(obs)])
}
lag_cor

plot(1:max_lag, lag_cor, type = "b",
     xlab = "Distance (km)",
     ylab = "Pearson Correlation",
     main = "Spatial Correlation vs Distance")
abline(h = 0, lty = 2)

acf(obs, lag.max = 5) # Compact solution!
#######################################################################




tmp |> filter(LogDistance==174)


ggplot(data=tmp2, aes(x=LogDistance, y=sum_nasc))+
  geom_col()
