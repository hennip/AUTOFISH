#
# Korvataan silakkamaaran sovitus silakkaosuuden sovituksella
# => binomijakauman approksimointi beta-jakaumalla
#

source("01-data/workflow-data.R")

MonkeyModel<-"
model{

  # Observation model for nautical area scattering coefficients
  ##############################################################
  for(i in 1:Nobs){
    NASC[i]~dlnorm(M_nasc[i], tau_nasc) 
    
    mu_nasc[i]<- (sigma[1]*n[LOG[i],1]+
                  sigma[2]*n[LOG[i],2]+
                  sigma[3]*n[LOG[i],3])/pA[i]
                  
    M_nasc[i]<-log(mu_nasc[i)-0.5*(1/tau_nasc)
    propA[LOG[i]]<-pA[i] # proportion of area i of total area
  }
  tau_nasc<-1/log(cv_nasc*cv_nasc+1)
  cv_nasc<-0.001#~dlnorm(0.03,3.26) # noise/ measurement error 
  
  # Abundances
  ############
  for(s in 1:Nspecies){
      Ntot[s]<-exp(Ntmp[s])
      Ntmp[s]~dnorm(13,0.0000001)
  }

  # Species composition in trawl catch
  ######################################
  for(h in 1:Nhaul){
    Sobs[1:Nspecies,h]~dmulti(qS[1:Nspecies,h],Cobs[h])
      
    # qS~ddirich() but  
    # approximate dirichlet (set of gamma distributions) with lognormal distns
    qS[1:Nspecies,h]<-zS[1:Nspecies,h]/sum(zS[1:Nspecies,h])

    for(s in 1:Nspecies){
        zS[s,h]~dlnorm(MS[s],tauS[s])
      }
  }
  
  for(s in 1:Nspecies){
    # muS[s]: expected proportion of species s
    muS[s]<-N[r,s]/sum(N[r,1:Nspecies])
  }
  MS[1:Nspecies]<-log(muS[1:Nspecies])-0.5*(1/tauS[1:Nspecies])
  tauS[1:Nspecies]<-1/log((1/alphaS[1:Nspecies])+1)
      
  alphaS[1:Nspecies]<-muS[1:Nspecies]*(etaS[1:Nspecies]+1)
      
  for(s in 1:Nspecies){
    
    # pE: probability that a fish of species s is at echo area i
    # E(pE[i]): proportion of echo area i compared to total area
    # etaE: overdispersion parameter
    pE[1:Necho,s]~ddirich(alphaE[1:Necho,s])
    alphaE[1:Necho,s]<-propA[1:Necho]*etaE[s] 
        
    for(e in 1:Necho){
      # n: number of fish of species s on echo area e of rectangle r
      n[e,s]<-Ntot[s]*pE[e,s]
    }
  }
  
  
    # Length data
   ############################################################################
    # For now, assume only known mean length for each species
    # # Observed number of fish of species s in each length class in rectangle r
    # Lobs[1:Nlengths[s],s]~dmulti(qL[1:Nlengths[s],s],nLobs[r,s])
    # 
    # # approximate dirichlet (set of gamma distributions) with lognormal distns
    # qL[1:Nlengths[s],s]<-zL[1:Nlengths[s],s]/
    #                           sum(zL[1:Nlengths[s],s])
    # 
    # for(l in 1:Nlengths[s]){
    #   zL[l,s]~dlnorm(ML[l,s],tauL[l,s])
    #   sigmaTmp[l,s]<-qL[l,s]*sigmaL[l,s]
    # }
    # sigma[s]<-sum(sigmaTmp[1:Nlengths[s],s])
    ############################################################################

  for(s in 1:Nspecies){
    #sigmaL[1:Nlengths[s],s]<-4*pi*pow(10,TSa/10)*pow(meanL[1:Nlengths[s],s],2)
    sigma[s]<-4*pi*pow(10,TSa/10)*pow(meanL[s],2)
  }
  TSa<- -71.2
      
    # Forget age estimates for now
    ############################################################################
    #   for(l in 1:Nlengths[1]){ # Age data on herring only
    #     # Age data
    #     #################
    #     # Gobs: observed number of herring of each age class in length class l
    #     Gobs[1:Nages,l]~dmulti(qG[1:Nages,l],nGobs[l])
    #     # qG: age distribution of length class l
    # 
    #     #qG~ddirich(alphaG[1:Nages,l]) but
    #     # approximate dirichlet (set of gamma distributions) with lognormal distns
    #     qG[1:Nages,l]<-zG[1:Nages,l]/sum(zG[1:Nages,l]) 
    #     
    #     for(a in 1:Nages){
    #       zG[a,l]~dlnorm(MG[a,l],tauG[a,l])
    #       pH_at_age[a,l]<-qL[l,1]*qG[a,l] # Proportion of herring at age on length
    #     }
    #   }
    #   for(a in 1:Nages){
    #     nH_at_age[a]<-sum(pH_at_age[a,1:Nlengths[1]])*N[r,1] # Number of herring at age
    #   }
    # }
    # 
    # for(a in 1:Nages){
    #   PopAge[a]<-sum(nH_at_age[a,1:Nrec])/Ntot[1]
    # }
    # for(l in 1:Nlengths[1]){
    #   alphaG[1:Nages,l]<-Gstar[1:Nages,l]*etaG
    #   Gstar[1:Nages,l]~ddirich(aG)
    #   MG[1:Nages,l]<-log(Gstar[1:Nages,l])-0.5*(1/tauG[1:Nages,l])
    #   tauG[1:Nages,l]<-1/log((1/alphaG[1:Nages,l])+1)
    # }
    ############################################################################
    
    
    
    for(s in 1:Nspecies){
      alphaL[1:Nlengths[s],s]<-Lstar[1:Nlengths[s],s]*(etaL[s]+1)
      ML[1:Nlengths[s],s]<-log(Lstar[1:Nlengths[s],s])-0.5*(1/tauL[1:Nlengths[s],s])
      tauL[1:Nlengths[s],s]<-1/log((1/alphaL[1:Nlengths[s],s])+1)
    }
    Lstar[1:Nlengths[1],1]~ddirich(aL1)
    Lstar[1:Nlengths[2],2]~ddirich(aL2)
    Lstar[1:Nlengths[3],3]~ddirich(aL3)
  }

  
#  etaG~dlnorm(0.8,0.1)

  for(s in 1:Nspecies){
    etaS[s]~dlnorm(0.8,0.1)
    etaR[s]~dlnorm(0.8,0.1)
    etaE[s]<-exp(etaEZ[s])
    etaEZ[s]~dnorm(13,0.0000001)  # this parameterisation may help with JAGS
    etaL[s]~dlnorm(0.8,0.1)
  }





}"

cat(GRAHS_model3,file="GRAHS3.txt")

#############################

# A_NM2<-c(819.8155089,# NW
#          1014.006703,# NE
#          536.3622401,# SW
#          1558.658342# SE
# )


data<-list(
  Nobs=4,
  Nhaul=1,
  
  Nyears=2,
  Nrec=4,
  Nages=10,
  Nspecies=3,
  Nlengths=c(N_lh,N_ls,N_lo),
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
  aL2=rep(1,N_ls),
  aL3=rep(1,N_lo),
  meanL=meanL/10 # mean lengths in cm's!!!
)

parnames=c(
  "muH",
  "PopAge",
  "Lstar",
  "cv_nasc", "cv_nascX", "etaX",
  "etaR", "etaE", "etaL","etaG","etaH", "etaS",
  "Ntot","N"
)

# 
run0<-run.jags(GRAHS_model3, monitor=parnames,data=data,n.chains = 2, method = 'parallel', thin=1,
               burnin =1000, modules = "mix",
               sample =1000, adapt = 1000,
               keep.jags.files=F,
               progress.bar=TRUE, jags.refresh=100)

t1<-Sys.time();print(t1)
run1<-run.jags(GRAHS_model3, monitor=parnames,data=data,n.chains = 2, 
               method = 'parallel', thin=100,
               burnin =10000, modules = "mix",
               sample =10000, adapt = 50000,
               keep.jags.files=F,
               progress.bar=TRUE, jags.refresh=100)
run<-run1
save(run, file="../out/GRAHS3.RData")
t2<-Sys.time();print(t2)
print("run1 done");print(difftime(t2,t1))
print("--------------------------------------------------")

plot(run2, var="eta")
chains<-as.mcmc.list(run)
#traceplot(chains[,"etaE[1]"])
summary(run, var="Ntot")



run2 <- extend.jags(run1, combine=F, sample=10000, thin=1000, keep.jags.files=F)
t3<-Sys.time();print(t3)
print("run2 done"); print(difftime(t3,t2))
print("--------------------------------------------------")
run<-run2
save(run, file="../out/GRAHS3.RData")

run3 <- extend.jags(run2, combine=T, sample=10000, thin=1000, keep.jags.files=F)
t4<-Sys.time();print(t4)
print("run3 done"); print(difftime(t4,t3))
print("--------------------------------------------------")
run<-run3
save(run, file="../out/GRAHS3.RData")

run4 <- extend.jags(run3, combine=T, sample=20000, thin=1000, keep.jags.files=F)
t5<-Sys.time();print(t5)
print("run4 done"); print(difftime(t5,t4))
print("--------------------------------------------------")
run<-run4
save(run, file="../out/GRAHS3.RData")

