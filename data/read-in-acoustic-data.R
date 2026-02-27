source("packages-and-paths.R")

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

# Sum nasc over different depths (in 1st version the depth is not accounted for)
tot_nasc_per_log<-dfA2 |> group_by(year, rec, LogDistance) |> 
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

# propA[1:Necho[r,y],r,y]


#Necho[r,y]
Necho_per_rec<-tot_nasc_per_log |> group_by(year, rec) |> summarise(n=n()) |> 
  pivot_wider(names_from = year, values_from = n) |> select(-rec)
Necho_per_rec

necho<-as.matrix(Necho_per_rec)

# nascY is year index fed to the model
nascY<-unlist(tot_nasc_per_log |>ungroup() |> select(year) |> mutate(year=year-2022), use.names = F)
nascY


# Area of each rectangle in square NM's
A_NM2<-c(819.8155089,# NW
     1014.006703,# NE
     536.3622401,# SW
     1558.658342# SE
)
rec_areas_NM2<-tibble(rec=1:4, A_NM2)

# We still one row of data per rectangle specifying the area that was NOT 
# covered by the survey track (i.e. more than 99.9% of it)

# Join rectangle areas, calculate proportion of the area observed (pA) for each LOG
pA1<-tot_nasc_per_log |>  ungroup()|> 
  select(year, rec, LOG, sum_nasc, area_NM2) |> 
  full_join(rec_areas_NM2) |> 
  mutate(pA=area_NM2/A_NM2) 
pA1

# Calculate the sum of the area covered per rectangle and proportion observed/not observed
pA2<-
  pA1|> 
  group_by(year, rec) |> 
  summarise(area_covered_NM2=sum(area_NM2)) |> 
  full_join(rec_areas_NM2) |> 
  mutate(area_not_covered_NM2=A-area_covered_NM2) |> 
  mutate(prop_covered=area_covered_NM2/A) |> 
  mutate(prop_not_covered=area_not_covered_NM2/A) |> 
  select(year, rec, A_NM2, everything())
pA2 |> as.data.frame()# print as data.frame to see all digits

# pick & rename the columns needed for the input data 
pA3<-pA2 |> select(year, rec, prop_not_covered) |>
  mutate(pA=prop_not_covered) |> select(-prop_not_covered)|> as.data.frame()
pA3

# Give NA NASC value and LOG number as max(LOG[r,y])+1
# -> this way the dimensions will match in the model

nasc_plus_one<-pA3 |> mutate(sum_nasc=NA)
log_plus_one<-tot_nasc_per_log |> summarise(max_LOG=max(LOG)) |> mutate(LOG=max_LOG+1)

 plus_one<-full_join(nasc_plus_one, log_plus_one) |> select(-max_LOG)

 tot_nasc_per_log_plus_one<-pA1 |> select(-area_NM2, -A_NM2) |> 
   full_join(plus_one)
 

# for(i in 1:Nobs){# total number of observations over years
#   NASC[i]~dlnorm(M_nasc[i,nascY[i]], tau_nasc) # NASC (m2/NM2) at depth 6-100m
#   # expected NASC at point i, year nascY[i]
#   mu_nasc[i,nascY[i]]<- (sigmaR[R[i],1,nascY[i]]*n[LOG[i],R[i],1,nascY[i]]+
#                            sigmaR[R[i],2,nascY[i]]*n[LOG[i],R[i],2,nascY[i]])/
#     (pA[i]*A[R[i]])
#   M_nasc[i,nascY[i]]<-log(mu_nasc[i,nascY[i]])-0.5*(1/tau_nasc)
#   propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
# }

#pA=  tot_nasc_per_log$area_NM2, # proportion of echo area out of total rectangle

# # Testing stuff
# ################################################################################
# 
# # psi: indicator of whether the depth is less than 25m
# tmp<-dfA2 |> mutate(psi=ifelse(depthLow>25, 1, 0)) |> group_by(LogDistance, psi, rec) |> 
#   summarise(nasc_tot=sum(DataValue))
# tmp
# # About 15% of observations is from deeper layers than 25m
# tmp |> group_by(psi) |> summarise(tot=sum(nasc_tot))
# 
# # Run minmax_depth from trawl data (code below)
# # delta: does the acoustic go lower than the trawl data in corresponding ruhne-rectangle?
# tmp2<-full_join(dfA2, minmax_depth) |> 
#   select(min_trawl_depth, max_trawl_depth, rec, DataValue, depthUpp, depthLow, LogDistance, LogLatitude, LogLongitude) |> 
#   arrange(rec) |> 
#   mutate(delta=ifelse(depthLow>max_trawl_depth, 1, 0))
# 
# View(tmp2)
# 
# 
# 
# # Calculate autocorrelation of nascs as the distance increases (chatGPT)
# #######################################################################
# obs <- tmp3$sum_nasc
# max_lag <- length(obs) - 1
# lag_cor <- numeric(max_lag)
# for (h in 1:max_lag) {
#   lag_cor[h] <- cor(obs[1:(length(obs)-h)], 
#                     obs[(1+h):length(obs)])
# }
# lag_cor
# 
# plot(1:max_lag, lag_cor, type = "b",
#      xlab = "Distance (km)",
#      ylab = "Pearson Correlation",
#      main = "Spatial Correlation vs Distance")
# abline(h = 0, lty = 2)
# 
# acf(obs, lag.max = 5) # Compact solution!
# #######################################################################
# 
# 
# 
# 
# tmp |> filter(LogDistance==174)
# 
# 
# ggplot(data=tmp2, aes(x=LogDistance, y=sum_nasc))+
#   geom_col()
