# These graphs are the ones in the BIAS manuscript 
#rm(list = ls())

source("00-basics/packages-and-paths.R")

load(paste0(path_output,"GRAHS_etaE_2020-2025.RData"))
load(paste0(path_output,"GRAHS4_etaE4etaR4_2020-2025.RData"))
load(paste0(path_output,"GRAHS4_cleaned_2020-2025.RData"))

load(paste0(path_output_GRAHS,"GRAHS4_cleaned_2016-2025.RData"))

summary(run, var="deviance")
plot(run, var="deviance")

summary(run, var="Ntot")
summary(run, var="eta")
#summary(run, var="PopAge")

plot(run, var="eta")
plot(run, var="cv_nasc")


summary(run, var="Lstar")


chains<-as.mcmc(run)

Nyears<-10
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

colnames(min)<-colnames(low)<-colnames(med)<-colnames(up)<-colnames(max)<-c(2016:2025)
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

Nspecies<-4
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

colnames(min)<-colnames(low)<-colnames(med)<-colnames(up)<-colnames(max)<-c(2016:2025)

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





















