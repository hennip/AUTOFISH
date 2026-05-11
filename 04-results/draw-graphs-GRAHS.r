# These graphs are the ones in the BIAS manuscript 
#rm(list = ls())

source("00-basics/packages-and-paths.R")
load(paste0(path_output,"GRAHS4.RData"))
summary(run, var="Ntot")
summary(run, var="eta")
windows()
plot(run, var="eta")


summary(run, var="eta")
summary(run, var="Ntot")
summary(run, var="N")
plot(run, var="Ntot")
plot(run, var="cv_nasc")
chains<-as.mcmc.list(run)
chains<-window(chains, start=2000000)
traceplot(chains[,"etaE[1]"])
traceplot(chains[,"etaE[2]"])
traceplot(chains[,"etaE[3]"])
traceplot(chains[,"etaE[4]"])
summary(chains[,"etaE[1]"])


chains<-as.mcmc.list(run)
chains<-window(chains,start=200000)
#summary(chains)

years<-c(2023:2024)
nyears<-length(years)

#ageOld<-read.table("data/der/AgeDist_oldEstimates.txt", header=T)
#source("prg/model/TrawlData/StandardLengthEstimates.r")
#LengthObs<-Pst

data<-list(
  Nyears=2,
  Nrec=4,
  Nages=10,
  Nspecies=4,
  Nlengths=c(N_lh,N_lsprat,N_lstickl,N_lo),
  pi=3.14159265358979323846,
  A=A_NM2, # Areas of rectangles, NM^2
  Atot=sum(A_NM2),
  
  NASC=tot_nasc_per_log_plus_one$sum_nasc, # All depths summed together for now
  R=   tot_nasc_per_log_plus_one$rec, # rectangle at log
  pA=  tot_nasc_per_log_plus_one$pA, # proportion of echo area out of total rectangle
  LOG= tot_nasc_per_log_plus_one$LOG,
  
  Nobs=length(tot_nasc_per_log_plus_one$sum_nasc), # Total number of observations over years
  Necho=necho+1, # number of echo areas = number of logs per rectangle+1 (+1 is the rest of the rec)  
  Nhaul=Nhaul, # Number of hauls per rectangle
  nascY=nascY, # Year index
  
  Cobs=C_obs, # Total catch per species
  Sobs=S_obs, # Number of individuals per species in each haul
  nLobs=nL_obs, # Sample size per length group
  Lobs=L_obs, # Number of individuals per length group in each sample
  Gobs=G_obs, # Number of individuals per age group in each sample
  nGobs=nG_obs, # sample size per age group
  aG=rep(1,10),
  aL1=rep(1,N_lh),
  aL2=rep(1,N_lsprat),
  aL3=rep(1,N_lstickl),
  aL4=rep(1,N_lo),
  meanL=meanL/10 # mean lengths in cm's!!!
)
summary(data)


#################
# Traces
par(mfrow=c(3,3),mar=c(2.5,4,4,1))
traceplot(chains[,"etaE[1]"],main=expression(eta[1]^E), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaE[2]"],main=expression(eta[2]^E), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaE[3]"],main=expression(eta[3]^E), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaE[4]"],main=expression(eta[4]^E), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"etaR[1]"],main=expression(eta[1]^R), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaR[2]"],main=expression(eta[2]^R), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaR[3]"],main=expression(eta[3]^R), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaR[4]"],main=expression(eta[4]^R), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"etaL[1]"],main=expression(eta[1]^L), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaL[2]"],main=expression(eta[2]^L), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaL[3]"],main=expression(eta[3]^L), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"etaL[4]"],main=expression(eta[4]^L), cex.main=1.5, col=c("black", "gray"))

traceplot(chains[,"etaG"],main=expression(eta^G), cex.main=1.5, col=c("black", "gray"))
traceplot(chains[,"cv_nasc"],main=expression(CV[nasc]), cex.main=1.2, col=c("black", "gray"))


plot(density(chains[,"cv_nasc"][[1]]),main=expression(CV[nasc]))
lines(density(chains[,"cv_nascX"][[1]]))

plot(density(chains[,"etaG"][[1]]),main=expression(eta^G))
lines(density(chains[,"etaX"][[1]]))

 
?traceplot

gelman.diag(chains[,"etaE[1]"])
gelman.diag(chains[,"etaE[2]"])
gelman.diag(chains[,"etaR[1]"])
gelman.diag(chains[,"etaR[2]"])
gelman.diag(chains[,"etaL[1]"])
gelman.diag(chains[,"etaL[2]"])
gelman.diag(chains[,"etaH"])
gelman.diag(chains[,"etaG"])
gelman.diag(chains[,"cv_nasc"])


par(mfrow=c(2,3),mar=c(2.5,4,4,1))
traceplot(chains[,"muH[1]"],main="muH[1]")
traceplot(chains[,"muH[2]"],main="muH[2]")
traceplot(chains[,"muH[3]"],main="muH[3]")
traceplot(chains[,"muH[4]"],main="muH[4]")
traceplot(chains[,"muH[5]"],main="muH[5]")
traceplot(chains[,"muH[6]"],main="muH[6]")

par(mfrow=c(4,7),mar=c(2.5,4,4,1))
for(y in 1:6){    
  for(r in 1:28){
    traceplot(chains[,paste(sep="","muH[",r,",",y,"]")],
    main=paste(sep="","muH[",r,",",y,"]"), ylim=c(0,1))
   # abline(h=data$HobsProp[r,y], col="cyan")                   
  }
}



par(mfrow=c(2,3),mar=c(2.5,4,4,1))
for(i in 1:nyears){
  traceplot(chains[,paste(sep="","Ntot[1,",i,"]")],
  main=vuosi[i], col=c("black", "gray"))           
}
par(mfrow=c(2,3),mar=c(2.5,4,4,1))
for(i in 1:nyears){
  traceplot(chains[,paste(sep="","Ntot[2,",i,"]")],
  main=vuosi[i], col=c("black", "gray"))           
}


par(mfrow=c(2,3),mar=c(2.5,4,4,1))
for(i in 1:nyears){
  print(gelman.diag(chains[,paste(sep="","Ntot[1,",i,"]")]))
}





par(mfrow=c(4,7),mar=c(2,4,3,1))
for(y in 1:nyears){
for(i in 1:28){
  traceplot(chains[,paste(sep="","N[",i,",1,",y,"]")],
  main=paste(sep="","N, r=",i," y=",y))
}
}


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


