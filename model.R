#
# Korvataan silakkamaaran sovitus silakkaosuuden sovituksella
# => binomijakauman approksimointi beta-jakaumalla
#
library(rjags)
library(runjags)

source("data/workflow-data.R")


GRAHS_model2<-"
model{

  # Observation model for nautical area scattering coefficients
  ##############################################################
  for(i in 1:Nobs){# total number of observations over years
    NASC[i]~dlnorm(M_nasc[i,nascY[i]], tau_nasc) # NASC (m2/NM2) at depth 6-100m
    # expected NASC at point i, year nascY[i]
    mu_nasc[i,nascY[i]]<- (sigmaR[R[i],1,nascY[i]]*n[LOG[i],R[i],1,nascY[i]]+
                           sigmaR[R[i],2,nascY[i]]*n[LOG[i],R[i],2,nascY[i]])/
                           (pA[i]*A[R[i]])
    M_nasc[i,nascY[i]]<-log(mu_nasc[i,nascY[i]])-0.5*(1/tau_nasc)
    propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
  }
  tau_nasc<-1/log(cv_nasc*cv_nasc+1)
  cv_nasc~dlnorm(0.03,3.26) # kohina/mittausvirhe, voidaan pit?? samana vuosien yli

  # Abundances
  ############
  for(s in 1:2){
    for(y in 1:Nyears){
      Ntot[s,y]<-exp(Ntmp[s,y])
      Ntmp[s,y]~dnorm(13,0.0000001)

      # pR: probability that a fish will be at rectangle r
      # E(pR[r])=A[r]/Atot: proportion of area in rectangle r compared to total
      # eta_R: overdispersion parameter, could be derived with schooling behaviour etc.
      # Ekspertit: onko kalojen jakauma ruuduille satunnainen, vai onko kalat todenn?k?isemmin
      # samalla ruudulla eri vuosina? pR:lle voisi tehd? rakenteen jossa vuosikohinaa mutta yleinen
      # tn osua tietylle ruudulle
      pR[1:Nrec,s,y]~ddirich(alphaR[1:Nrec,s])
    }
    alphaR[1:Nrec,s]<-(A[1:Nrec]/Atot)*etaR[s] # Palautettu kässärin muotoon, jossa ruudun osuuden odotusarvo sama yli vuosien
  }

  for(y in 1:Nyears){
    for(r in 1:Nrec){

      # Proportion of herring in trawl catch
      ######################################
      for(h in 1:Nhaul[r,y]){ # Several hauls per ruhne rectangle
        HobsProp[h,r,y]~dbeta(aH[h,r,y],bH[h,r,y])
        aH[h,r,y]<-muH[r,y]*Cobs[h,r,y]*etaH
        bH[h,r,y]<-(1-muH[r,y])*Cobs[h,r,y]*etaH
      }
      muH[r,y]<-N[r,1,y]/(N[r,1,y]+N[r,2,y])
      
      for(s in 1:2){
        # N: Number of fish of species s on rectangle r
        N[r,s,y]<-Ntot[s,y]*pR[r,s,y]

        # pE satunnainen vuosien yli
        # pE: probability that a fish at rectangle r is at echo area i
        # E(pE[i,r]): proportion of echo area i compared to total area of rectangle r
        # etaE: overdispersion parameter
        pE[1:Necho[r,y],r,s,y]~ddirich(alphaE[1:Necho[r,y],r,s,y])
        alphaE[1:Necho[r,y],r,s,y]<-propA[1:Necho[r,y],r,y]*etaE[s] # etaE palautettu kässärin muotoon, sama yli vuosien

        for(e in 1:Necho[r,y]){
          # n: number of fish of species s on echo area e of rectangle r
          n[e,r,s,y]<-N[r,s,y]*pE[e,r,s,y]
        }

        # Length data
        #################
        # Observed number of fish of species s in each length class in rectangle r
        Lobs[1:Nlengths[s],r,s,y]~dmulti(qL[1:Nlengths[s],r,s,y],nLobs[r,s,y])

        # approximate dirichlet (set of gamma distributions) with lognormal
        #qL[1:8,r,s,y]~ddirich(alphaL[1:8,s,y]) # length distributions
        qL[1:Nlengths[s],r,s,y]<-zL[1:Nlengths[s],r,s,y]/
                                  sum(zL[1:Nlengths[s],r,s,y])

        for(l in 1:Nlengths[s]){
          zL[l,r,s,y]~dlnorm(ML[l,s,y],tauL[l,s,y])
          sigmaTmp[l,r,s,y]<-qL[l,r,s,y]*sigmaL[l,s]
        }
        sigmaR[r,s,y]<-sum(sigmaTmp[1:Nlengths[s],r,s,y])
      }

      for(l in 1:Nlengths[1]){ # Age data on herring only
        # Age data
        #################
        # Gobs: observed number of herring of each age class in length class l
        Gobs[1:Nages,l,r,y]~dmulti(qG[1:Nages,l,r,y],nGobs[l,r,y])
        # qG: age distribution of length class l

        # approximate dirichlet (set of gamma distributions) with lognormal
        qG[1:Nages,l,r,y]<-zG[1:Nages,l,r,y]/sum(zG[1:Nages,l,r,y]) #~ddirich(alphaG[1:Nages,l,y])

        for(a in 1:Nages){
          zG[a,l,r,y]~dlnorm(MG[a,l,y],tauG[a,l,y])
          pH_at_age[a,l,r,y]<-qL[l,r,1,y]*qG[a,l,r,y] # Proportion of herring at age on length
        }
      }
      for(a in 1:Nages){
        nH_at_age[a,r,y]<-sum(pH_at_age[a,1:Nlengths[1],r,y])*N[r,1,y] # Number of herring at age
      }
    }

    for(a in 1:Nages){
      PopAge[a,y]<-sum(nH_at_age[a,1:Nrec,y])/Ntot[1,y]
    }
    for(l in 1:8){
      alphaG[1:Nages,l,y]<-Gstar[1:Nages,l,y]*etaG
      Gstar[1:Nages,l,y]~ddirich(aG)
      MG[1:Nages,l,y]<-log(Gstar[1:Nages,l,y])-0.5*(1/tauG[1:Nages,l,y])
      tauG[1:Nages,l,y]<-1/log((1/alphaG[1:Nages,l,y])+1)
    }
    for(s in 1:2){
      alphaL[1:Nlengths[s],s,y]<-Lstar[1:Nlengths[s],s,y]*(etaL[s]+1)
      ML[1:Nlengths[s],s,y]<-log(Lstar[1:Nlengths[s],s,y])-0.5*(1/tauL[1:Nlengths[s],s,y])
      tauL[1:Nlengths[s],s,y]<-1/log((1/alphaL[1:Nlengths[s],s,y])+1)
    }
    Lstar[1:Nlengths[1],1,y]~ddirich(aL1)
    Lstar[1:Nlengths[2],2,y]~ddirich(aL2)
  }

  for(s in 1:2){
    # meanL: midpoint of each length class
    sigmaL[1:Nlengths[s],s]<-4*pi*pow(10,TSa/10)*pow(meanL[1:Nlengths[s],s],2)
  }
  TSa<- -71.2
  
  #etaH~dbeta(2,2)
  #etaH_tmp~dnorm(0,0.5) 
  #etaH<-exp(etaH_tmp)/(1+exp(etaH_tmp))# logit-normal similar as beta(1,1)
  etaH~dlnorm(0.8,0.1)
  etaG~dlnorm(0.8,0.1)

  for(s in 1:2){
    etaR[s]~dlnorm(0.8,0.1)
    #etaE_tmp[s]~dnorm(0,0.5) 
    #etaE[s]<-exp(etaE_tmp)/(1+exp(etaE_tmp))# logit-normal similar as beta(1,1)
    #etaE[s]~dlnorm(0.8,0.1)
    etaE[s]<-exp(etaEZ[s])
    etaEZ[s]~dnorm(13,0.0000001)  # this parameterisation may help with JAGS
    etaL[s]~dlnorm(0.8,0.1)
  }

# ajattele eta otoskokona joka jaetaan eri luokkiin dir-jakaumassa.
# spatiaalisen vaihtelun maara, voitaisiin ehka pitaa samana vuosien yli (ainakin alkuun)
# mita pienempi etaR, sita v?hemm?n kalat jakautuneet ruutujen pinta-alan mukaan.
# myohemmin voitais tehda tama niin etta etaR riippuu kalojen maarasta 
# -> vahan kalaa, suurempi keskittyminen samoille paikoille.
# etaE voisi periaatteessa riippua kalojen maarasta, mutta pidetaan nyt samana yli vuosien


  # Unupdated priors
  ##############################
  NTX<-exp(NtmpX)
  NtmpX~dnorm(13,0.0000001)
  cv_nascX~dlnorm(0.03,3.26)
  etaX~dlnorm(0.8,0.1)


}"

cat(GRAHS_model2,file="GRAHS2.txt")

#############################


data<-list(
  Nyears=2,
  Nrec=4,
  Nages=10,
  Nlengths=c(8,6),
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
  HobsProp=Hprops, # Proportion of herring in the catch
  nLobs=nL_obs, # Sample size per length group
  Lobs=L_obs, # Number of individuals per length group in each sample
  Gobs=G_obs, # Number of individuals per age group in each sample
  nGobs=nG_obs, # sample size per age group
  aG=rep(1,10),
  aL1=rep(1,8),
  aL2=rep(1,6),
  meanL=meanL/10 # mean lengths in cm's
)

parnames=c(
  "muH",
  "PopAge",
  "Lstar",
  "cv_nasc", "cv_nascX", "etaX",
  "etaR", "etaE", "etaL","etaG","etaH",
  "Ntot","N"
)

# 
# run0<-run.jags(GRAHS_model2, monitor=parnames,data=data,n.chains = 2, method = 'parallel', thin=1,
#          burnin =1000, modules = "mix",
#          sample =1000, adapt = 1000,
#          keep.jags.files=F,
#          progress.bar=TRUE, jags.refresh=100)

t1<-Sys.time();print(t1)
run1<-run.jags(GRAHS_model2, monitor=parnames,data=data,n.chains = 2, 
               method = 'parallel', thin=100,
               burnin =10000, modules = "mix",
               sample =10000, adapt = 50000,
               keep.jags.files=F,
               progress.bar=TRUE, jags.refresh=100)
run<-run1
save(run, file="../out/GRAHS2.RData")
t2<-Sys.time();print(t2)
print("run1 done");print(difftime(t2,t1))
print("--------------------------------------------------")

plot(run, var="eta")
#chains<-as.mcmc.list(run)
#traceplot(chains[,"etaE[1]"])
summary(run, var="Ntot")



run2 <- extend.jags(run1, combine=F, sample=10000, thin=1000, keep.jags.files=F)
t3<-Sys.time();print(t3)
print("run2 done"); print(difftime(t3,t2))
print("--------------------------------------------------")
run<-run2
save(run, file="../out/GRAHS2.RData")

run3 <- extend.jags(run2, combine=T, sample=10000, thin=1000, keep.jags.files=F)
t4<-Sys.time();print(t4)
print("run3 done"); print(difftime(t4,t3))
print("--------------------------------------------------")
run<-run3
save(run, file="../out/GRAHS2.RData")

run4 <- extend.jags(run3, combine=T, sample=20000, thin=1000, keep.jags.files=F)
t5<-Sys.time();print(t5)
print("run4 done"); print(difftime(t5,t4))
print("--------------------------------------------------")
run<-run4
save(run, file="../out/GRAHS1.RData")

