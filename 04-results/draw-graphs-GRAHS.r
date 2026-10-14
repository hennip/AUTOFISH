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
species_name<-c("Herring", "Sprat", "Stickleback", "Other")

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

#load(paste0(path_output_GRAHS,"GRAHS20_14lengths_2020-2025.RData"))

#load(paste0(path_output_GRAHS,"GRAHS20_Snew_2020-2025.RData"))
#load(paste0(path_output_GRAHS,"GRAHS20_wAge_Snew_2016-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS20_Nunif_EtaEsy_Snew_2020-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS20_EtaEsy_CVnasc_2020-2025.RData"))


################################################################################
# DIAGNOSTICS: TRACES
################################################################################

chains<-as.mcmc.list(run)
chains<-window(chains, start=200000)

# 
# 
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

options(max.print=100000)
summary(run, var="sigmaR")

summary(run, var="deviance")

summary(run, var="mu_nasc[2772]") # rec 1, y1
summary(run, var="mu_nasc[2773]") # rec 2, y1
summary(run, var="mu_nasc[2774]") # rec 3, y1
summary(run, var="mu_nasc[2775]") # rec 4, y1
summary(run, var="mu_nasc[2776]") # rec 1, y2
summary(run, var="mu_nasc[2777]") # rec 2, y2
summary(run, var="mu_nasc[2778]") # rec 3, y2
summary(run, var="mu_nasc[2779]") # rec 4, y2


plot(run, var="deviance")
summary(run, var="muL")
summary(run, var="cv_nasc")
plot(run, var="cv_nasc")

summary(run, var="N")
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

#data$Sobs[,,4,6]


chains<-as.mcmc(run)
#chains<-window(chains, start=200000)

# Predicted nasc
for(i in 2772:2795){
  tmp<-summary(chains[,paste0("mu_nasc[",i,"]")], quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
  if(i==2772){df_nasc_pred<-tmp}
  if(i>2772){
    df_nasc_pred<-rbind(df_nasc_pred,tmp)}
}
df_pred<-as_tibble(df_nasc_pred) |> 
  mutate(rec=rep(c(1,2,3,4),6), year=c(rep(2020,4),rep(2021,4),rep(2022,4),rep(2023,4),rep(2024,4),rep(2025,4)))


nasc_obs<-tot_nasc_per_log_plus_one$sum_nasc
nasc_rec<-tot_nasc_per_log_plus_one$rec
nascY

df_dat<-as.tibble(cbind(nasc_obs, nasc_rec, nascY)) |> 
  rename(rec=nasc_rec, year=nascY) |> 
  mutate(year=year+2019)

df_med_nasc<- df_dat |> group_by(rec, year) |> summarise(med=mean(nasc_obs, na.rm=T))

ggplot(df_pred, aes(rec, group = rec))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = `5%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `95%`),
    stat = "identity",fill=rgb(1,1,1,0.1), color="red")+
  coord_cartesian(ylim=c(0,15000))+
  facet_wrap(~year)+
  geom_point(data=df_dat, aes(x=rec, y=nasc_obs), alpha=0.1)+
  geom_point(data=df_med_nasc, aes(x=rec, y=med), size=5, alpha=0.2)



 
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

par(mfrow=c(2,2),mar=c(2.5,4,4,1))
plot(density(chains[,"etaE[1]"]),main=expression(eta[1]^E));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaE[2]"]),main=expression(eta[2]^E));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaE[3]"]),main=expression(eta[3]^E));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaE[4]"]),main=expression(eta[4]^E));  lines(density(chains[,"etaX2"]))


par(mfrow=c(2,2),mar=c(2.5,4,4,1))
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
chains<-as.mcmc(run)

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

# 4 species in a loop
p<-list()
for(s in 1:4){

  min2<-minS[s,,]
  low2<-lowS[s,,]
  med2<-medS[s,,]
  up2<-upS[s,,]
  max2<-maxS[s,,]

  colnames(min2)<-colnames(low2)<-colnames(med2)<-colnames(up2)<-colnames(max2)<-Years
  
  df_min<-as_tibble(min2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "min")
  df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
  df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "med")
  df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
  df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "max")
  
  df1<-full_join(df_min, df_low)|> 
    full_join(df_med)|> 
    full_join(df_up) |> 
    full_join(df_max)|> 
    mutate(species=s)

  df_catch_tot<-dfB_catch_all_species |> group_by(year, rec_ruhnu, HaulNumber) |> 
    summarise(catch_tot=sum(catch3))

  df_p<-dfB_catch_all_species |> left_join(df_catch_tot) |> 
    mutate(p_catch=catch3/catch_tot) |> ungroup() |>
    rename(rec=rec_ruhnu) |> select(year,rec,species,p_catch)

  df2<-df_p|> filter(species==s) |> select(-species)
  
p[[s]]<-ggplot(df1, aes(rec, group=rec))+
  labs(x="Length group", y="Proportion per species", 
       title=paste0("Proportion of ",species_name[s]))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  coord_cartesian(ylim=c(0,1))+
  geom_point(data=df2, aes(x=rec, y=p_catch))
}
p[[1]]
p[[2]]
p[[3]]
p[[4]]



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

# 4 species in a loop
p<-list()
y_lim<-c(20000,11000,21000,1700)
for(s in 1:4){
  
  min2<-minR[s,,]
  low2<-lowR[s,,]
  med2<-medR[s,,]
  up2<-upR[s,,]
  max2<-maxR[s,,]
  
  colnames(min2)<-colnames(low2)<-colnames(med2)<-colnames(up2)<-colnames(max2)<-Years
  
  df_min<-as_tibble(min2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "min")
  df_low<-as_tibble(low2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
  df_med<-as_tibble(med2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "med")
  df_up<-as_tibble(up2) |> mutate(rec=row_number()) |>  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
  df_max<-as_tibble(max2) |> mutate(rec=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "max")
  
  df1<-full_join(df_min, df_low)|> 
    full_join(df_med)|> 
    full_join(df_up) |> 
    full_join(df_max)|> 
    mutate(species=species_name[s])

p[[s]]<-ggplot(df1, aes(rec, group=rec))+
  labs(x="Ruhnu rectangle", y="Year", 
       title=paste0("Abundance of ",species_name[s]," per rec (in millions)"))+
  coord_cartesian(ylim=c(0,y_lim[s]))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")
}
p[[1]]
p[[2]]
p[[3]]
p[[4]]



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

colnames(min)<-colnames(low)<-colnames(med)<-colnames(up)<-colnames(max)<-Years

df_min<-as_tibble(min) |> mutate(species=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "min")
df_low<-as_tibble(low) |> mutate(species=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "low") 
df_med<-as_tibble(med) |> mutate(species=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "med")
df_up<-as_tibble(up) |> mutate(species=row_number()) |>  pivot_longer(1:Nyears,names_to = "year", values_to = "up")
df_max<-as_tibble(max) |> mutate(species=row_number()) |> pivot_longer(1:Nyears,names_to = "year", values_to = "max")

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
# Proportion at length per species
######################################

Nlengths<-c(N_lh,N_lsprat,N_lstickl,N_lo)
#Nlengths<-c(14,8,8,8)

min<-low<-med<-up<-max<-array(max(Nlengths), dim=c(14,max(Nlengths),Nspecies,Nyears))
for(y in 1:Nyears){
  for(r in 1:Nrec){
    for(s in 1:Nspecies){
      for(l in 1:Nlengths[s]){
        tmp<-chains[,str_c("qL[",l,",",s,",",r,",",y,"]")]
        
        sum_tmp<-summary(tmp, quantiles=c(0.05,0.25,0.5,0.75,0.95))$quantiles
        min[l,s,r,y]<-sum_tmp[1]
        low[l,s,r,y]<-sum_tmp[2]
        med[l,s,r,y]<-sum_tmp[3]
        up[l,s,r,y]<-sum_tmp[4]
        max[l,s,r,y]<-sum_tmp[5]
      }
    }
  }
}

df_p_at_length<-numbers_per_length_group |> 
  left_join(sample_size) |> 
  mutate(p_at_length=number_at_length/tot_sample) |> 
  select(year, species, rec_ruhnu, HaulNumber, length_group, p_at_length) |> 
  rename(rec=rec_ruhnu)

df_p_at_length

# Now need loops over Nyears for each species
s<-2
p<-list()
for(y in 1:6){
  min2<-min[,s,,y]
  low2<-low[,s,,y]
  med2<-med[,s,,y]
  up2<-up[,s,,y]
  max2<-max[,s,,y]
  
  colnames(min2)<-colnames(low2)<-colnames(med2)<-colnames(up2)<-colnames(max2)<-c(1:4)
  
  df_min<-as_tibble(min2) |> mutate(length=row_number()) |> pivot_longer(1:Nrec,names_to = "rec", values_to = "min")
  df_low<-as_tibble(low2) |> mutate(length=row_number()) |> pivot_longer(1:Nrec,names_to = "rec", values_to = "low") 
  df_med<-as_tibble(med2) |> mutate(length=row_number()) |> pivot_longer(1:Nrec,names_to = "rec", values_to = "med")
  df_up<-as_tibble(up2) |> mutate(length=row_number()) |>  pivot_longer(1:Nrec,names_to = "rec", values_to = "up")
  df_max<-as_tibble(max2) |> mutate(length=row_number()) |> pivot_longer(1:Nrec,names_to = "rec", values_to = "max")
  
  df1<-full_join(df_min, df_low)|> 
    full_join(df_med)|> 
    full_join(df_up) |> 
    full_join(df_max)|> 
    mutate(species=s, year=y)
  
  # df_catch_tot<-dfB_catch_all_species |> group_by(year, rec_ruhnu, HaulNumber) |> 
  #   summarise(catch_tot=sum(catch3))
  # 
  # df_p<-dfB_catch_all_species |> left_join(df_catch_tot) |> 
  #   mutate(p_catch=catch3/catch_tot) |> ungroup() |>
  #   rename(rec=rec_ruhnu) |> select(year,rec,species,p_catch)
  # 
  # df2<-df_p|> filter(species==s) |> select(-species)

  df2<-df_p_at_length |> filter(year==y+min_year-1, species==s) |> 
    rename(length=length_group) |> select(-year, -species)
 
  p[[y]]<-
    ggplot(df1, aes(length, group=length))+
    labs(x="Length group", y="Proportion per length group", 
         title=paste0("Proportion per length group, ",species_name[s], " ", y+min_year-1))+
    theme_bw()+
    geom_boxplot(
      aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
      stat = "identity",fill=rgb(1,1,1,0.1))+
    facet_wrap(~rec, scales="free")+
    coord_cartesian(ylim=c(0,1), xlim=c(0, Nlengths[s]+2))+
    geom_point(data=df2, aes(x=length, y=p_at_length, colour = factor(HaulNumber)))
}
p[[1]]
p[[2]]
p[[3]]
p[[4]]
p[[5]]
p[[6]]

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

# 4 species in a loop
p<-list()
for(s in 1:4){
  min2<-min[,s,]
  low2<-low[,s,]
  med2<-med[,s,]
  up2<-up[,s,]
  max2<-max[,s,]

  colnames(min2)<-colnames(low2)<-colnames(med2)<-
    colnames(up2)<-colnames(max2)<-Years

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
    mutate(species=species_name[s])

  df2<-filter(df_comp, species==s) |> select(-species) |> 
    mutate(year=as.numeric(year))
  df<-df1 |> mutate(year=as.numeric(year))|> 
    full_join(df2)

#df1 |> filter(year==2017) |> select(-species, -length, -low, -up)
#print(x=df1, n=100)

#windows(record=T)
p[[s]]<-ggplot(df, aes(length, group=length))+
  labs(x="Length group", y="Abundance per length group", 
       title=paste0("Total abundance per length group, ",species_name[s]))+
  theme_bw()+
  coord_cartesian(xlim=c(0,Nlengths[s]+1))+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  geom_point(aes(length, N)) #Excel calculated values

}
p[[1]]
p[[2]]
p[[3]]
p[[4]]



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
  colnames(max)<-Years
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

