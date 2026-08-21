# These graphs are the ones in the BIAS manuscript 
#rm(list = ls())

source("00-basics/packages-and-paths.R")

load(paste0(path_output,"GRAHS_etaE_2020-2025.RData"))
load(paste0(path_output,"GRAHS4_etaE4etaR4_2020-2025.RData"))
load(paste0(path_output,"GRAHS4_cleaned_2020-2025.RData"))

load(paste0(path_output,"GRAHS4_cleaned_2016-2025.RData"))

summary(run, var="deviance")
plot(run, var="deviance")

summary(run, var="Ntot")
summary(run, var="eta")
summary(run, var="PopAge")

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

for(y in 1:Nyears){
  plot(density(chains[,str_c("etaS[",y,"]")]),main=bquote(.(y+2019) ~ eta^S))
  lines(density(chains[,"etaX1"]))
}

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
plot(density(chains[,"etaR[1]"]),main=expression(eta[1]^R));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaR[2]"]),main=expression(eta[2]^R));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaR[3]"]),main=expression(eta[3]^R));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaR[4]"]),main=expression(eta[4]^R));  lines(density(chains[,"etaX"]))

plot(density(chains[,"etaE[1]"]),main=expression(eta[1]^E));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaE[2]"]),main=expression(eta[2]^E));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaE[3]"]),main=expression(eta[3]^E));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaE[4]"]),main=expression(eta[4]^E));  lines(density(chains[,"etaX"]))


par(mfrow=c(3,3),mar=c(2.5,4,4,1))
plot(density(chains[,"etaL[1]"]),main=expression(eta[1]^L));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaL[2]"]),main=expression(eta[2]^L));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaL[3]"]),main=expression(eta[3]^L));  lines(density(chains[,"etaX"]))
plot(density(chains[,"etaL[4]"]),main=expression(eta[4]^L));  lines(density(chains[,"etaX"]))

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





















# length dist, herring
par(mfrow=c(3,8),mar=c(2.5,4,4,1))
for(y in 1:nyears){
for(i in 1:8){
  traceplot(chains[,paste(sep="","Lstar[",i,",1,",y,"]")],
  main=paste(sep="","Lstar, LC=",i,", ",vuosi[y]))

  print(y)
  print(i)
    print(gelman.diag(chains[,paste(sep="","Lstar[",i,",1,",y,"]")]))

}
}



# Age dist
par(mfrow=c(3,9),mar=c(2.5,4,4,1))
for(y in 1:nyears){
for(a in 1:9){
  traceplot(chains[,paste(sep="","PopAge[",a,",",y,"]")],
  main=paste(sep="","PopAge, age=",a,", ",vuosi[y]))

  print(y)
  print(a)

    print(gelman.diag(chains[,paste(sep="","PopAge[",a,",",y,"]")]))

}
}
####################
# Herring proportions 
par(mfrow=c(2,3),mar=c(2.5,4,4,1))
#densityplot(chains[,"pH[1]"],main="pH[1]")
for(y in 1:6){
  plot(density(chains[,paste(sep="","muH[",y,"]")][[1]]),
  main=paste(sep="","muH[",y,"]"), xlim=c(0.1,1))
  points(density(chains[,paste(sep="","muH[",y,"]")][[2]]),col=2, type="l")
  #points(data$HobsProp[,y],rep(0,28)
  for(r in 1:28){ 
    points(data$HobsProp[r,y],0, #cex=(data$Cobs[r,y]/10000))
    #cex=y)
    cex=sqrt(data$Cobs[r,y]/1000))
  }
}

par(mfrow=c(4,7),mar=c(2.5,4,4,1))
for(y in 1:6){
for(r in 1:28){
  plot(density(chains[,paste(sep="","muH[",r,",",y,"]")][[1]]),
  main=paste(sep="","muH[",r,",",y,"]"), xlim=c(0,1))
  points(density(chains[,paste(sep="","muH[",r,",",y,"]")][[2]]),col=2, type="l")
  points(data$HobsProp[r,y],0, pch=16, cex=2)
}
}

par(mfrow=c(4,7),mar=c(2.5,4,4,1))
for(y in 1:6){
for(r in 1:28){
  plot(density(chains[,paste(sep="","HobsProp[",r,",",y,"]")][[1]]),
  main=paste(sep="","HobsProp[",r,",",y,"]"), xlim=c(0,1))
  points(density(chains[,paste(sep="","HobsProp[",r,",",y,"]")][[2]]),col=2, type="l")
  points(data$HobsProp[r,y],0, pch=16, cex=2)
}
}
data$HobsProp
data$Cobs

####################


niter<-length(chains[,"etaH"][[1]])

# Abundances
A.1<-array(NA, dim=c(5,nyears))
A.2<-array(NA, dim=c(5,nyears))
Ntot1<-array(NA, dim=c(nyears,niter*2))
for(y in 1:nyears){
#y<-1
    Ntot1[y,]<-as.mcmc(c(chains[,paste(sep="","Ntot[1,",y,"]")][[1]],
                              chains[,paste(sep="","Ntot[1,",y,"]")][[2]]))
  A.1[,y]<-summary( as.mcmc(c(chains[,paste(sep="","Ntot[1,",y,"]")][[1]],
                              chains[,paste(sep="","Ntot[1,",y,"]")][[2]])))$quantiles
  A.2[,y]<-summary( as.mcmc(c(chains[,paste(sep="","Ntot[2,",y,"]")][[1]],
                              chains[,paste(sep="","Ntot[2,",y,"]")][[2]])))$quantiles
}
cv<-c()
for(y in 1:6){
  cv[y]<-summary(as.mcmc(Ntot1[y,]))$statistics[2]/
  summary(as.mcmc(Ntot1[y,]))$statistics[1]
}
cv

# length/age dist
Q.L.1<-array(NA, dim=c(5,nyears,8)) # quantiles, years, classes
Q.L.2<-array(NA, dim=c(5,nyears,8))
Q.G.1<-array(NA, dim=c(5,nyears,9))

for(y in 1:nyears){
  for(i in 1:8){
  Q.L.1[,y,i]<-summary( as.mcmc(c(
                chains[,paste(sep="","Lstar[",i,",1,",y,"]")][[1]],
                chains[,paste(sep="","Lstar[",i,",1,",y,"]")][[2]])))$quantiles
  Q.L.2[,y,i]<-summary( as.mcmc(c(
                chains[,paste(sep="","Lstar[",i,",2,",y,"]")][[1]],
                chains[,paste(sep="","Lstar[",i,",2,",y,"]")][[2]])))$quantiles
  }
  for(i in 1:9){
    Q.G.1[,y,i]<-summary( as.mcmc(c(
                  chains[,paste(sep="","PopAge[",i,",",y,"]")][[1]],
                  chains[,paste(sep="","PopAge[",i,",",y,"]")][[2]])))$quantiles
  }
}

# Length and age compositions  2009-2012
####################
  par(mfrow=c(2,1))#,mar=c(4.5,4.5,3,0.8))
  k<-c("<7.5","7.5-10","10-12.5","12.5-15","15-17.5",
  "17.5-20","20-22.5",">22.5")

  # Length
  sP<- -0.1
  plot(1:8+sP,Q.L.1[3,3,], pch=16, ylim=c(0,0.8), ylab="Proportion",
  xlab="Length group (cm)",bty="l", xlim=c(0.5,8.5),
  main="Length composition",
  xaxt="n", cex=1,cex.lab=1.2, cex.main=1.2, cex.axis=1.2)
  segments(1:8+sP,Q.L.1[1,3,],1:8+sP,Q.L.1[5,3,])#, lwd=2)
  axis(side=1, at=c(1:8), labels=k, cex.axis=1.2)

  for(i in 4:6){
    points(1:8+(sP+(i-3)*0.1),Q.L.1[3,i,], pch=16,col=i-2)
    segments(1:8+(sP+(i-3)*0.1),Q.L.1[1,i,],1:8+(sP+(i-3)*0.1),Q.L.1[5,i,], col=i-2)
  }
  for(i in 4:6){
    points(1:8+(sP+(i-3)*0.1),LengthObs[,i-3], col=i-2, pch=1, cex=1.2)
  }
  legend("topleft", col=c(1:4,1), legend=c(2009:2012, "Standard estimates"),
  pch=c(rep(16,4),1), lty=c(rep(1,4),NA), pt.cex=c(rep(1,4),1.2))

# Separate plots
par(mfrow=c(2,2),mar=c(4.5,4.5,3,0.8))
k<-c("<7.5","7.5-10","10-12.5","12.5-15","15-17.5","17.5-20","20-22.5",">22.5")

for(y in 3:6){
  plot(1:8,Q.L.1[3,y,], pch=16, ylim=c(0,0.8), ylab="Proportion",
  xlab=" ",xlim=c(0.5,8.5), bty="l",main=vuosi[y],
  xaxt="n",cex=1.2,cex.lab=1.2, cex.main=1.5, cex.axis=1.2)
  segments(1:8,Q.L.1[1,y,],1:8,Q.L.1[5,y,])#, lwd=2)
  axis(side=1, at=c(1:8), labels=k, cex.axis=1, las=2)
  if(y>3){points(1:8,LengthObs[,y-3], col=1, pch=2, cex=1.5)}
}





  # Age
  sP<- -0.1
  plot(0:8+sP,Q.G.1[3,3,], pch=16, ylim=c(0,0.5), ylab="Proportion",
  bty="l",xlab="Age group", main="Age composition", xlim=c(-0.5,8.5),
  xaxt="n", cex=1,cex.lab=1.2, cex.main=1.2, cex.axis=1.2)
  segments(0:8+sP,Q.G.1[1,3,],0:8+sP,Q.G.1[5,3,])
  age<-c(0:7,"8+")
  axis(side=1, at=c(0:8), labels=age, cex.axis=1.2)
  points(0:8+sP,ageOld[,3], pch=1, col="black", cex=1.2)
  #text(0,0.38, labels="(A)", cex=1.5)

  for(i in 4:6){
  #i<-2
    points(0:8+(sP+(i-3)*0.1),Q.G.1[3,i,], pch=16,col=i-2)
    segments(0:8+(sP+(i-3)*0.1),Q.G.1[1,i,],0:8+(sP+(i-3)*0.1),Q.G.1[5,i,], col=i-2)

    points(0:8+(sP+(i-3)*0.1),ageOld[,i], pch=1, col=i-2, cex=1.2)
  }
  legend(x=5, y=0.5, col=c(1:4,1), legend=c(2009:2012, "Standard estimates"),
  pch=c(rep(16,4),1), lty=c(rep(1,4),NA), pt.cex=c(rep(1,4),1.2))

# Separate plots
par(mfrow=c(2,2),mar=c(4.5,4.5,3,0.8))
  age<-c(0:7,"8+")
for(y in 3:6){
  plot(0:8,Q.G.1[3,y,], pch=16, ylim=c(0,0.5), ylab="Proportion",
  xlab=" ",xlim=c(0.5,8.5), bty="l",main=vuosi[y],
  xaxt="n",cex=1.2,cex.lab=1.2, cex.main=1.5, cex.axis=1.2)
  segments(0:8,Q.G.1[1,y,],0:8,Q.G.1[5,y,])#, lwd=2)
  axis(side=1, at=c(0:8), labels=age, cex.axis=1.2)
  points(0:8,ageOld[,y], col=1, pch=2, cex=1.5)
}


  ##################

  # Abundance
years<-vuosi
DivideWith<-1000000000
aOld<-c(24.25235, 22.89570, 27.51380, 25.94337, 35.14212, 29.77686)
xmin<-c(8,8,25,8)
xmax<-c(30,30,41,30)
#Ntmp<-read.table("prg/output/Ntmp_coda1000.txt")
#Ntot_prior<-exp(Ntmp)
#Ntot_prior<-as.numeric(Ntot_prior[[1]])/DivideWith
A.1/DivideWith
aOld

1-((A.1[3,]/DivideWith)/aOld)

A.1[3,]/DivideWith
aOld*0.75


windows()
par(mfrow=c(2,3))#,mar=c(4,4,4,1))
for(y in 1:6){
  plot(density(Ntot1[y,]/DivideWith),
  xlab="Abundance in billions (10^9)", 
  main=(years)[y], #xlim=c(xmin[y], xmax[y]), 
  cex.lab=1.3, cex.axis=1.2, cex.main=1.5)
  abline(v=aOld[y], col="red")
  #points(density(Ntot_prior), type="l", lty=2)
}










plot(years,A.1[3,]/DivideWith, pch=16, main="Herring abundance",
xlab="Abundance in billions (10^9)", ylim=c(0,4e+10/DivideWith),
cex.lab=1.2, cex.main=1.2, cex.axis=1.2, cex=1.2)
segments(years, A.1[1,]/DivideWith, years, A.1[5,]/DivideWith, lty=1)#, col="grey")
segments(years, A.1[2,]/DivideWith, years, A.1[4,]/DivideWith, lwd=1)

# vanhat estimaatit, miljoonaa silakkaa
aOld<-c(24.25235, 22.89570, 27.51380, 25.94337, 35.14212, 29.77686)
points(2007:2012,aOld[1:6], col="red", pch=1, cex=1.2)
legend("bottomleft", pch=c(16,1), col=c(1,2), pt.cex=1.2,
legend=c("Bayesian estimate","Standard estimate"), lty=c(1,NA))

plot(years,A.2[3,]/DivideWith, pch=16, main="Abundance of other species",
ylab="Abundance in billions (10^9)", xlab="Year", ylim=c(0,10e+10/DivideWith),
cex.lab=1.2, cex.main=1.2, cex.axis=1.2, cex=1.2)
segments(years, A.2[1,]/DivideWith, years, A.2[5,]/DivideWith, lty=1)#, col="grey")
segments(years, A.2[2,]/DivideWith, years, A.2[4,]/DivideWith, lwd=1)


