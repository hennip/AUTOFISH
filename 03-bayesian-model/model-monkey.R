#
# Korvataan silakkamaaran sovitus silakkaosuuden sovituksella
# => binomijakauman approksimointi beta-jakaumalla
#

source("01-data/workflow-data.R")

MonkeyModel<-"
model{

  # Observation model for NASCs
  ########################################
  for(i in 1:Nobs){ # i: index for nascs
    NASC[i]~dlnorm(M_nasc[i], tau_nasc) 
    
    # Expected NASC is calculated as combination of species specific sigmas,
    # species specific abundances and proportion of the area covered. 
    # Now PA is always 1/16
    mu_nasc[i]<- (sigma[1]*n[LOG[i],1]+
                  sigma[2]*n[LOG[i],2]+
                  sigma[3]*n[LOG[i],3])/pA[i]
                  
    M_nasc[i]<-log(mu_nasc[i)-0.5*(1/tau_nasc)
    propA[LOG[i]]<-pA[i] # proportion of area i of total area
  }
  tau_nasc<-1/log(cv_nasc*cv_nasc+1)
  cv_nasc<-0.001 # noise/ measurement error, assume now known but small 
  
  # Total Abundance per species
  ########################################
  for(s in 1:Nspecies){ # s: index for species
      Ntot[s]~dunif(10,10000) # uniform prior for this example
  }

  # Species composition in trawl catch
  ########################################
  for(h in 1:Nhaul){ # h: index for haul number
    # Sobs: observed number per species in the trawl catch per species
    # Cobs: total trawl catch
    # qS: relative proportion of each species (eg. c(0.3,0.3,0.4)) in haul h
    Sobs[1:Nspecies,h]~dmulti(qS[1:Nspecies,h],Cobs[h])
      
    # qS~ddirich() but for computational reasons we approximate dirichlet distribution 
    # with a set of lognormal distns (technical, monkey can ignore this)
    qS[1:Nspecies,h]~ddirich(muS[1:Nspecies])
    # qS[1:Nspecies,h]<-zS[1:Nspecies,h]/sum(zS[1:Nspecies,h])
    # for(s in 1:Nspecies){
    #     zS[s,h]~dlnorm(MS[s],tauS[s])
    #   }
  }
  
 # Species composition in total population
 ###########################################
 for(s in 1:Nspecies){ # s: index for species
    # muS[s]: expected proportion of species s is its relative share in the total population
    muS[s]<-n[s]/Ntot[s]
  }
  # # Parameters MS and tauS link the species composition between observed hauls and the total population
  # MS[1:Nspecies]<-log(muS[1:Nspecies])-0.5*(1/tauS[1:Nspecies])
  # tauS[1:Nspecies]<-1/log((1/alphaS[1:Nspecies])+1)
  # #alphaS[1:Nspecies]<-muS[1:Nspecies]*etaS
      
  # Spatial model: Dirichlet process (like binomial (eg coin toss) but with 16 potential outcomes) 
  ###########################################
  for(s in 1:Nspecies){ # s: index for species
    
    # pE: probability that a fish of species s is at echo area i
    # E(pE[i]): proportion of echo area i compared to total area
    # etaE: overdispersion parameter
    pE[1:Necho,s]~ddirich(alphaE[1:Necho,s])
    alphaE[1:Necho,s]<-propA[1:Necho]*Ntot[s]*etaE
        
    for(e in 1:Necho){
      # n: true number of fish of species s on subarea e. Last subarea is the part that was not visited
      n[e,s]<-Ntot[s]*pE[e,s]
    }
  }
  
  # Sigma: Assume now only one length per species (meanL)
  ########################################################
  for(s in 1:Nspecies){
    sigma[s]<-4*pi*pow(10,-71.2/10)*pow(meanL[s],2)
  }

  # Overdispersion parameters
#  etaS~dlnorm(0.8,0.1)
  etaE~dunif(0.001,1)


}"

cat(MonkeyModel,file="MonkeyModel.txt")

#############################


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

