rm(list = ls())
source("00-basics/packages-and-paths.R")

# Define time series for data
min_year<-2020
max_year<-2025
Nyears=length(min_year:max_year)
model_data<-str_c("_14lengths_",min_year,"-",max_year)
source("01-data/workflow-data-bayesmodel.R")


# Choose model
source("03-bayesian-model/model_4species_clean.R")
#source("03-bayesian-model/model_4species_etaE.R")
#source("03-bayesian-model/model_4species_etaE4etaR4.R")



run_name<-str_c(modelname, model_data)

data<-list(
  Nyears=Nyears,
  Nrec=4,
  Nages=Nages, # Herring 0 - 8+ yr olds => 9 age groups 
  
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
  aG=rep(1,Nages),
  aL1=rep(1,N_lh),
  aL2=rep(1,N_lsprat),
  aL3=rep(1,N_lstickl),
  aL4=rep(1,N_lo),
  meanL=meanL/10 # mean lengths in cm's!!!
)

parnames=c(
  "deviance",
  "muS",
  "ageH",
  "muL",
  "cv_nasc", "cv_nascX", "etaX", "etaX1", "etaX2",
  "etaR", "etaE", "etaL","etaG","etaS",
  "Ntot","N", "NTX"
)

sink(paste0("sink_",run_name,"_",".txt"))
#sink()
run_name

t1<-Sys.time();print(t1)
run1<-run.jags(modelname, monitor=parnames,data=data,n.chains = 2, 
               #inits=inits,
               method = 'parallel', thin=100,
               burnin =10000, modules = "mix",
               sample =10000, adapt = 50000,
               keep.jags.files=F,
               progress.bar=TRUE, jags.refresh=100)
run<-run1
save(run, file=paste0(path_output,run_name,".RData"))
t2<-Sys.time();print(t2)
print("run1 done");print(difftime(t2,t1))
print("--------------------------------------------------")

run2 <- extend.jags(run1, combine=T, sample=10000, thin=100, keep.jags.files=F)
t3<-Sys.time();print(t3)
print("run2 done"); print(difftime(t3,t2))
print("--------------------------------------------------")
run<-run2
save(run, file=paste0(path_output,run_name,".RData"))


t31<-Sys.time();print(t31)
run3 <- extend.jags(run2, combine=T, sample=10000, thin=100, keep.jags.files=F)
t32<-Sys.time();print(t32)
print("run3 done"); print(difftime(t31,t32))
print("--------------------------------------------------")
run<-run3
save(run, file=paste0(path_output,run_name,".RData"))

t41<-Sys.time();print(t41)
run4 <- extend.jags(run3, combine=T, sample=10000, thin=100, keep.jags.files=F)
t42<-Sys.time();print(t42)
print("run4 done"); print(difftime(t41,t42))
print("--------------------------------------------------")
run<-run4
save(run, file=paste0(path_output,run_name,".RData"))


sink()
