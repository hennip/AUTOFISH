# These graphs are the ones in the BIAS manuscript 
#rm(list = ls())

source("00-basics/packages-and-paths.R")

load(paste0(path_output,"GRAHS_etaE_2020-2025.RData"))
load(paste0(path_output,"GRAHS4_etaE4etaR4_2020-2025.RData"))
load(paste0(path_output,"GRAHS4_cleaned_2020-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS4_cleaned_2016-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS4_cleaned_14lengths_2016-2025.RData"))
load(paste0(path_output_GRAHS,"GRAHS4_etaEry_14lengths_2020-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS4_etaEry_etaSry_14lengths_2020-2025.RData"))

summary(run, var="deviance")
plot(run, var="deviance")

summary(run, var="Ntot")
summary(run, var="etaS")

plot(run, var="etaS")
plot(run, var="cv_nasc")
summary(run, var="muL")


chains<-as.mcmc(run)

Nyears<-6#10
Nages<-9
Nspecies<-4
species_name<-c("Herring", "Sprat", "Stickleback", "Other")

#################
# Prior vs posterior
par(mfrow=c(3,3),mar=c(2.5,4,4,1))

plot(density(chains[,"cv_nasc"]),main=expression(CV[nasc]))
lines(density(chains[,"cv_nascX"]))

plot(density(chains[,"etaG"]),main=expression(eta^G))
lines(density(chains[,"etaX1"]))
#plot(density(chains[,"etaX"]),main=expression(eta^X), xlim=c(0,4000))

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
for(y in 1:Nyears){
  plot(density(chains[,str_c("etaS[",y,"]")]),main=bquote(.(y+2019) ~ eta^S))
  lines(density(chains[,"etaX1"]), col="red")
}

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
plot(density(chains[,"etaR[1]"]),main=expression(eta[1]^R));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaR[2]"]),main=expression(eta[2]^R));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaR[3]"]),main=expression(eta[3]^R));  lines(density(chains[,"etaX2"]))
plot(density(chains[,"etaR[4]"]),main=expression(eta[4]^R));  lines(density(chains[,"etaX2"]))

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
  }
}


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
  colnames(up)<-colnames(max)<-c(2020:2025)

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
df

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
      p<-chains[,str_c("muL[",l,",",s,",",y,"]")]
      N<-chains[,str_c("Ntot[",s,",",y,"]")] 
      tmp<-p*N/1000000
      
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

ggplot(df, aes(length, group=length))+
  labs(x="Length group", y="Abundance per length group", title="Total abundance per length group, herring")+
  theme_bw()+
  geom_boxplot(
    aes(ymin = min, lower = low, middle = med, upper = up, ymax = max),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  facet_wrap(~year, scales="free")+
  geom_point(aes(length, N))


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
  #coord_cartesian(ylim = c(0, 1500))+
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

