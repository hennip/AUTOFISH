# These graphs are the ones in the BIAS manuscript 
#rm(list = ls())

source("00-basics/packages-and-paths.R")
# Define time series for data
min_year<-2020
max_year<-2025
Years<-c(min_year:max_year)
Nyears=length(Years)
Nages<-9
Nspecies<-4
Nrec<-4

model_data<-str_c("_14lengths_",min_year,"-",max_year)
source("01-data/workflow-data-bayesmodel.R")



# load(paste0(path_output_GRAHS,"GRAHS4_ind_muL_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_ind_muL_qS_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_ind_qS_qL_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_ind_qS_qL_simple_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_ind_muL_2_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_ind_muL_2_muRsr_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_N_14lengths_2020-2025.RData"))
# load(paste0(path_output_GRAHS,"GRAHS4_N_qS_14lengths_2020-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS20_14lengths_2020-2025.RData"))




################################################################################
# DIAGNOSTICS: TRACES
################################################################################

chains<-as.mcmc.list(run)
chains<-window(chains, start=200000)



cs<-varnames(chains)
length(cs) #23689

pdf("traces2.pdf")
par(mfrow=c(3,3))

for( i in cs){
  cat("\n===Käsittelee ",i)
  gd<-gelman.diag(chains[,i])
  if(is.na(gd$psrf[2])==F){
    if(gd$psrf[2]>1.1){
      traceplot(chains[,i],main=paste(i))
    }
  }
}
dev.off()

# Pring psrf's >1.1
for( i in cs){
  gd<-gelman.diag(chains[,i])
  if(is.na(gd$psrf[2])==F){
    if(gd$psrf[2]>1.1){
      cat("\n===",i)
      print(gd$psrf[2])
    }
  }
}


summary(run, var="deviance")
plot(run, var="deviance")
summary(run, var="muL")
summary(run, var="cv_nasc")

summary(run, var="Ntot")
summary(run, var="etaR")

plot(run, var="Ntot")
windows(record = T)
plot(run, var="eta")
plot(run, var="etaS")
plot(run, var="cv_nasc")

summary(run, var="N[3,2,1]")
summary(run, var="N[2,1,6]")

summary(run, var="N[2,1,6]")
summary(run, var="N[2,2,6]")
summary(run, var="N[2,3,6]")
summary(run, var="N[2,4,6]")
summary(run, var="muS[2,4,6]")

summary(run, var="etaS")
summary(run, var="qS[1,4,6]")
summary(run, var="qS[2,4,6]")
summary(run, var="qS[3,4,6]")
summary(run, var="qS[4,4,6]")

data$Sobs[,,4,6]


chains<-as.mcmc(run)
chains<-window(chains, start=200000)
species_name<-c("Herring", "Sprat", "Stickleback", "Other")

################################################################################
# DIAGNOSTICS: TRACES AND PRIOR VS POSTERIOR
################################################################################
par(mfrow=c(3,3),mar=c(2.5,4,4,1))

plot(density(chains[,"cv_nasc"]),main=expression(CV[nasc]))
lines(density(chains[,"cv_nascX"]))

plot(density(chains[,"etaG"]),main=expression(eta^G))
lines(density(chains[,"etaX1"]))
#plot(density(chains[,"etaX"]),main=expression(eta^X), xlim=c(0,4000))

# par(mfrow=c(3,3),mar=c(2.5,4,4,1))
# for(y in 1:Nyears){
#   plot(density(chains[,str_c("etaS[",y,"]")]),main=bquote(.(y+2019) ~ eta^S))
#   lines(density(chains[,"etaX1"]), col="red")
# }

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
# plot(density(chains[,"etaR[1]"]),main=expression(eta[1]^R));  lines(density(chains[,"etaX2"]))
# plot(density(chains[,"etaR[2]"]),main=expression(eta[2]^R));  lines(density(chains[,"etaX2"]))
# plot(density(chains[,"etaR[3]"]),main=expression(eta[3]^R));  lines(density(chains[,"etaX2"]))
# plot(density(chains[,"etaR[4]"]),main=expression(eta[4]^R));  lines(density(chains[,"etaX2"]))

plot(density(chains[,"etaE[1]"]),main=expression(eta[1]^E));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaE[2]"]),main=expression(eta[2]^E));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaE[3]"]),main=expression(eta[3]^E));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaE[4]"]),main=expression(eta[4]^E));  lines(density(chains[,"etaX2"]))


par(mfrow=c(3,3),mar=c(2.5,4,4,1))
plot(density(chains[,"etaL[1]"]),main=expression(eta[1]^L));  lines(density(chains[,"etaX1"]), col="red")
plot(density(chains[,"etaL[2]"]),main=expression(eta[2]^L));  lines(density(chains[,"etaX1"]), col="red")
plot(density(chains[,"etaL[3]"]),main=expression(eta[3]^L));  lines(density(chains[,"etaX1"]), col="red")
plot(density(chains[,"etaL[4]"]),main=expression(eta[4]^L));  lines(density(chains[,"etaX1"]), col="red")

par(mfrow=c(2,3),mar=c(2.5,4,4,1))
for(s in 1:Nspecies){
  for(y in 1:Nyears){
  plot(density(chains[,str_c("Ntot[",s,",",y,"]")]/1e+06), main=str_c(species_name[s]," ", y+2019))
  lines(density(chains[,str_c("NTX")]/1e+06))
  }
}

################################################################################
# PLOTS: ESTIMATES VS INPUT DATA
################################################################################

# Species composition vs S_obs
# =========================================
S_obs; dim(S_obs) 
# Sobs[1:Nspecies,h,r,y]
# qS[1:Nspecies,h,r,y]

Nhaul # Number of hauls per rec and year

minS<-lowS<-medS<-upS<-maxS<-array(NA, dim=c(Nspecies,Nrec,Nyears))
for(y in 1:Nyears){
  for(r in 1:Nrec){
    for(s in 1:Nspecies){
      #for(h in 1:Nhaul[r,y]){
        tmp<-chains[,str_c("qS[",s,",",r,",",y,"]")]
        #tmp<-chains[,str_c("muS[",s,",",r,",",y,"]")]
        
        sum_tmp<-summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
        minS[s,r,y]<-sum_tmp[1]
        lowS[s,r,y]<-sum_tmp[2]
        medS[s,r,y]<-sum_tmp[3]
        upS[s,r,y]<-sum_tmp[4]
        maxS[s,r,y]<-sum_tmp[5]
      #}
    }
  }
}


# herring
min2<-minS[1,,]
low2<-lowS[1,,]
med2<-medS[1,,]
up2<-upS[1,,]
max2<-maxS[1,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years


df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species=1)

df_catch_tot<-dfB_catch_all_species |> group_by(year, rec_ruhnu, HaulNumber) |> 
  summarise(catch_tot=sum(catch3))

df_p<-dfB_catch_all_species |> left_join(df_catch_tot) |> 
  mutate(p_catch=catch3/catch_tot) |> ungroup() |>
  rename(rec=rec_ruhnu) |> 
  select(year,rec,species,p_catch)

df2<-df_p|> 
  filter(species==1) |> select(-species)
  
#windows(record=T)
ggplot(df1, aes(rec, group=rec))+
  labs(x="Length group", y="Proportion per species", 
       title="Proportion of herring")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  coord_cartesian(ylim=c(0,1))+
  geom_point(data=df2, aes(x=rec, y=p_catch))
  
# sprat
min2<-minS[2,,]
low2<-lowS[2,,]
med2<-medS[2,,]
up2<-upS[2,,]
max2<-maxS[2,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years

df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species=2)

df2<-df_p|> 
  filter(species==2) |> select(-species)

ggplot(df1, aes(rec, group=rec))+
  labs(x="Length group", y="Proportion per species", 
       title="Proportion of sprat")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  coord_cartesian(ylim=c(0,1))+
  geom_point(data=df2, aes(x=rec, y=p_catch))

# gta
min2<-minS[3,,]
low2<-lowS[3,,]
med2<-medS[3,,]
up2<-upS[3,,]
max2<-maxS[3,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years

df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species=3)

df2<-df_p|> 
  filter(species==3) |> select(-species)

ggplot(df1, aes(rec, group=rec))+
  labs(x="Length group", y="Proportion per species", 
       title="Proportion of stickleback")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  coord_cartesian(ylim=c(0,1))+
  geom_point(data=df2, aes(x=rec, y=p_catch))

################################################################################
# PLOTS: ESTIMATES, NO INPUT DATA FOR COMPARISON
################################################################################

######################################
# Rectangle specific abundance per species
######################################

minR<-lowR<-medR<-upR<-maxR<-array(NA, dim=c(Nspecies,Nrec,Nyears))
for(y in 1:Nyears){
  for(r in 1:Nrec){
    for(s in 1:Nspecies){
      tmp<-chains[,str_c("N[",s,",",r,",",y,"]")]
      sum_tmp<-summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
      minR[s,r,y]<-sum_tmp[1]/1e+06
      lowR[s,r,y]<-sum_tmp[2]/1e+06
      medR[s,r,y]<-sum_tmp[3]/1e+06
      upR[s,r,y]<-sum_tmp[4]/1e+06
      maxR[s,r,y]<-sum_tmp[5]/1e+06
    }
  }
}

# Herring
min2<-minR[1,,]
low2<-lowR[1,,]
med2<-medR[1,,]
up2<-upR[1,,]
max2<-maxR[1,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years

df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species="herring")


#windows()
ggplot(df1, aes(rec, group=rec))+
  labs(x="Ruhnu rectangle", y="Year", title="Abundance of herring per rec (in millions)")+
  #coord_cartesian(ylim=c(0,60000))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")

# Sprat
min2<-minR[2,,]
low2<-lowR[2,,]
med2<-medR[2,,]
up2<-upR[2,,]
max2<-maxR[2,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years

df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species="sprat")


#windows()
ggplot(df1, aes(rec, group=rec))+
  labs(x="Ruhnu rectangle", y="Year", title="Abundance of sprat per rec (in millions)")+
  #coord_cartesian(ylim=c(0,60000))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")

# gta
min2<-minR[3,,]
low2<-lowR[3,,]
med2<-medR[3,,]
up2<-upR[3,,]
max2<-maxR[3,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years

df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species="gta")


#windows()
ggplot(df1, aes(rec, group=rec))+
  labs(x="Ruhnu rectangle", y="Year", title="Abundance of stickleback per rec (in millions)")+
  #coord_cartesian(ylim=c(0,60000))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")

# other
min2<-minR[4,,]
low2<-lowR[4,,]
med2<-medR[4,,]
up2<-upR[4,,]
max2<-maxR[4,,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-Years

df_min<-as_tibble(min2) |> mutate(rec=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species="gta")


#windows()
ggplot(df1, aes(rec, group=rec))+
  labs(x="Ruhnu rectangle", y="Year", title="Abundance of other species per rec (in millions)")+
  coord_cartesian(ylim=c(0,1250))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")


######################################
# Total abundance per species
######################################

min<-low<-med<-up<-max<-array(NA, dim=c(Nspecies,Nyears))
for(y in 1:Nyears){
  for(s in 1:Nspecies){
    tmp<-chains[,str_c("Ntot[",s,",",y,"]")]
    sum_tmp<-summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
    min[s,y]<-sum_tmp[1]/1e+06
    low[s,y]<-sum_tmp[2]/1e+06
    med[s,y]<-sum_tmp[3]/1e+06
    up[s,y]<-sum_tmp[4]/1e+06
    max[s,y]<-sum_tmp[5]/1e+06
  }
}

colnames(min)<-colnames(low)<-colnames(med)<-
  colnames(up)<-colnames(max)<-Years

df_min<-as_tibble(min) |> mutate(species=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low) |> mutate(species=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med) |> mutate(species=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up) |> mutate(species=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max) |> mutate(species=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df<-full_join(df_min, df_low) |> 
  full_join(df_med) |> 
  full_join(df_up) |> 
  full_join(df_max) |> 
  mutate(species2=ifelse(species==1, "Herring", 
                         ifelse(species==2, "Sprat",
                                ifelse(species==3, "Stickleback",
                                       ifelse(species==4, "Other",NA))))) |> 
  arrange(species)

ggplot(df, aes(year, group=year))+
  labs(x="Species", y="Year", title="Total abundance per species (in millions)")+
  #coord_cartesian(ylim=c(0,60000))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~species2, scales="free")+
  expand_limits(y = 0)

######################################
# Abundance at length per species
######################################

df_compare_herring<-read_xlsx(str_c(path_output_GRAHS,"GOR_output_compiled_lengths.xlsx"), range="N22:X36")
df_compare_sprat<-read_xlsx(str_c(path_output_GRAHS,"GOR_output_compiled_lengths.xlsx"), range="N40:X48")
df_compare_gta<-read_xlsx(str_c(path_output_GRAHS,"GOR_output_compiled_lengths.xlsx"), range="N51:X59")

df_compiled_lengths<-full_join(df_compare_herring, df_compare_sprat) |> 
  full_join(df_compare_gta)

#df_compiled_lengths<-read_xlsx(str_c(path_output_GRAHS,"GOR_output_compiled_lengths.xlsx"), range="M40:W57")
df_comp<-df_compiled_lengths |> 
  pivot_longer(cols=c(`2017`:`2025`), names_to = "year", values_to = "N")|> rename(length=length_g)


df_comp<-df_compiled_lengths |> select(length_g, species,`2020`:`2025`) |>
  pivot_longer(cols=c(`2020`:`2025`), names_to = "year", values_to = "N")|> 
  rename(length=length_g)


Nlengths<-c(N_lh,N_lsprat,N_lstickl,N_lo)
#Nlengths<-c(14,8,8,8)

min<-low<-med<-up<-max<-array(NA, dim=c(max(Nlengths),Nspecies,Nyears))
for(y in 1:Nyears){
  for(s in 1:Nspecies){
    for(l in 1:Nlengths[s]){
      p1<-chains[,str_c("qL[",l,",",s,",",1,",",y,"]")]
      N1<-chains[,str_c("N[",s,",",1,",",y,"]")] 
      p2<-chains[,str_c("qL[",l,",",s,",",2,",",y,"]")]
      N2<-chains[,str_c("N[",s,",",2,",",y,"]")]
      p3<-chains[,str_c("qL[",l,",",s,",",3,",",y,"]")]
      N3<-chains[,str_c("N[",s,",",3,",",y,"]")]
      p4<-chains[,str_c("qL[",l,",",s,",",4,",",y,"]")]
      N4<-chains[,str_c("N[",s,",",4,",",y,"]")]

      tmp<-(p1*N1+p2*N2+p3*N3+p4*N4)/1000000
      #tmp<-(p1*N1)/1000000
      
      sum_tmp<-summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
      min[l,s,y]<-sum_tmp[1]
      low[l,s,y]<-sum_tmp[2]
      med[l,s,y]<-sum_tmp[3]
      up[l,s,y]<-sum_tmp[4]
      max[l,s,y]<-sum_tmp[5]
    }
  }
}


# herring
min2<-min[,1,]
low2<-low[,1,]
med2<-med[,1,]
up2<-up[,1,]
max2<-max[,1,]


colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-c(2020:2025)#c(2016:2025)


df_min<-as_tibble(min2) |> mutate(length=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(length=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low)|> 
  full_join(df_med)|> 
  full_join(df_up) |> 
  full_join(df_max)|> 
  mutate(species="herring")

df2<-filter(df_comp, species==1) |> select(-species) |> mutate(year=as.numeric(year))

df<-df1 |> mutate(year=as.numeric(year))|> 
  full_join(df2)

df1 |> filter(year==2017) |> select(-species, -length, -low, -up)
print(x=df1, n=100)

windows(record=T)
ggplot(df, aes(length, group=length))+
  labs(x="Length group", y="Abundance per length group", title="Total abundance per length group, herring")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  geom_point(aes(length, N)) #Excel calculated values

# Sprat

min2<-min[,2,]
low2<-low[,2,]
med2<-med[,2,]
up2<-up[,2,]
max2<-max[,2,]


colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-c(2020:2025)

df_min<-as_tibble(min2) |> mutate(length=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(length=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low) |> 
  full_join(df_med) |> 
  full_join(df_up) |> 
  full_join(df_max) |> 
  mutate(species="sprat")

df2<-filter(df_comp, species==2) |> select(-species) |> mutate(year=as.numeric(year))
df<-df1 |> mutate(year=as.numeric(year))|> 
  full_join(df2) |> 
  filter(is.na(min)==F)
#View(df)

ggplot(df, aes(length, group=length))+
  labs(x="Length group", y="Abundance per length group", title="Total abundance per length group, sprat")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+ 
  #coord_cartesian(ylim = c(0, 1000))+
  facet_wrap(~year, scales="free")+
  geom_point(aes(length, N))


# GTA

min2<-min[,3,]
low2<-low[,3,]
med2<-med[,3,]
up2<-up[,3,]
max2<-max[,3,]


colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-c(2020:2025)

df_min<-as_tibble(min2) |> mutate(length=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(length=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low) |> 
  full_join(df_med) |> 
  full_join(df_up) |> 
  full_join(df_max) |> 
  mutate(species="GTA")

df2<-filter(df_comp, species==3) |> select(-species) |> mutate(year=as.numeric(year))
df<-df1 |> mutate(year=as.numeric(year))|> 
  full_join(df2) |> 
  filter(is.na(min)==F)
#View(df)

ggplot(df, aes(length, group=length))+
  labs(x="Length group", y="Abundance per length group", title="Total abundance per length group, stickleback")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  geom_point(aes(length, N))


# Other

min2<-min[,4,]
low2<-low[,4,]
med2<-med[,4,]
up2<-up[,4,]
max2<-max[,4,]

colnames(min2)<-colnames(low2)<-colnames(med2)<-
  colnames(up2)<-colnames(max2)<-c(2020:2025)

df_min<-as_tibble(min2) |> mutate(length=row_number()) |>
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up2) |> mutate(length=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max2) |> mutate(length=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df1<-full_join(df_min, df_low) |> 
  full_join(df_med) |> 
  full_join(df_up) |> 
  full_join(df_max) |> 
  mutate(species="GTA")

df2<-filter(df_comp, species==4) |> select(-species) |> mutate(year=as.numeric(year))
df<-df1 |> mutate(year=as.numeric(year))|> 
  full_join(df2) |> 
  filter(is.na(min)==F)
#View(df)

ggplot(df, aes(length, group=length))+
  labs(x="Length group", y="Abundance per length group", title="Total abundance per length group, other species")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  geom_point(aes(length, N))

##########################################
# Relative species composition
##########################################



######################################
# Herring abundance per age group
######################################


min<-low<-med<-up<-max<-array(NA, dim=c(Nages,Nyears))
for(y in 1:Nyears){
  for(i in 1:Nages){
    
    p<-chains[,str_c("ageH[",i,",",y,"]")]
    N<-chains[,str_c("Ntot[1,",y,"]")] #1: herring
    tmp<-p*N/1000000
    sum_tmp<-summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
    min[i,y]<-sum_tmp[1]
    low[i,y]<-sum_tmp[2]
    med[i,y]<-sum_tmp[3]
    up[i,y]<-sum_tmp[4]
    max[i,y]<-sum_tmp[5]
  }
}

colnames(min)<-colnames(low)<-colnames(med)<-colnames(up)<-
  colnames(max)<-c(2020:2025)
max

df_min<-as_tibble(min) |> mutate(age=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low) |> mutate(age=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med) |> mutate(age=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up) |> mutate(age=row_number()) |>  
  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max) |> mutate(age=row_number()) |> 
  pivot_longer(1:Nyears,names_to = "year", values_to = "max")

df<-full_join(df_min, df_low) |> 
  full_join(df_med) |> 
  full_join(df_up) |> 
  full_join(df_max)

df<-df |> mutate(age=age-1)

ggplot(df, aes(age, group=age))+
  labs(x="Age class", y="Number of herring", title="Herring abundance per age (GRAHS)")+
  #coord_cartesian(xlim=c(0.5,9.4))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year)+scale_x_continuous(breaks = scales::pretty_breaks(n = 9))

