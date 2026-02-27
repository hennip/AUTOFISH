#
# Korvataan silakkam??r?n sovitus silakkaosuuden sovituksella
# => binomijakauman approksimointi beta-jakaumalla
#
library(rjags)
library(runjags)

source("data/read-in-acoustic-data.R")
source("data/read-in-trawl-data.R")




GRAHS_model1<-"
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
  for(y in 1:Nyears){
    for(s in 1:2){
      Ntot[s,y]<-exp(Ntmp[s,y])
      Ntmp[s,y]~dnorm(13,0.0000001)

      # pR: probability that a fish will be at rectangle r
      # E(pR[r])=A[r]/Atot: proportion of area in rectangle r compared to total
      # eta_pR: overdispersion parameter, could be derived with schooling behaviour etc.
      # Ekspertit: onko kalojen jakauma ruuduille satunnainen, vai onko kalat todenn?k?isemmin
      # samalla ruudulla eri vuosina? pR:lle voisi tehd? rakenteen jossa vuosikohinaa mutta yleinen
      # tn osua tietylle ruudulle
      pR[1:Nrec,s,y]~ddirich(alphaR[1:Nrec,s,y])
      alphaR[1:Nrec,s,y]<-(A[1:Nrec]/Atot)*etaR[y,s]
    }


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
        alphaE[1:Necho[r,y],r,s,y]<-propA[1:Necho[r,y],r,y]*etaE[y,s]

        for(e in 1:Necho[r,y]){
          # n: number of fish of species s on echo area e of rectangle r
          n[e,r,s,y]<-N[r,s,y]*pE[e,r,s,y]
        }

        # Length data
        #################
        # Observed number of fish of species s in each length class in rectangle r
        Lobs[1:8,r,s,y]~dmulti(qL[1:8,r,s,y],nLobs[r,s,y])

        # approximate dirichlet (set of gamma distributions) with lognormal
        #qL[1:8,r,s,y]~ddirich(alphaL[1:8,s,y]) # length distributions
        qL[1:8,r,s,y]<-zL[1:8,r,s,y]/sum(zL[1:8,r,s,y])

        for(l in 1:8){
          zL[l,r,s,y]~dlnorm(ML[l,s,y],tauL[l,s,y])
          sigmaTmp[l,r,s,y]<-qL[l,r,s,y]*sigmaL[l]
        }
        sigmaR[r,s,y]<-sum(sigmaTmp[1:8,r,s,y])
      }

      for(l in 1:8){
        # Age data
        #################
        # Gobs: observed number of fish of each age class in length class l
        Gobs[1:Nages,l,r,y]~dmulti(qG[1:Nages,l,r,y],nGobs[l,r,y])
        # qG: age distribution of length class l

        # approximate dirichlet (set of gamma distributions) with lognormal
        qG[1:Nages,l,r,y]<-zG[1:Nages,l,r,y]/sum(zG[1:Nages,l,r,y]) #~ddirich(alphaG[1:Nages,l,y])

        for(a in 1:Nages){
          zG[a,l,r,y]~dlnorm(MG[a,l,y],tauG[a,l,y])
          qGtmp1[a,l,r,y]<-qL[l,r,1,y]*qG[a,l,r,y]
        }
      }
      for(a in 1:Nages){
        qGtmp2[a,r,y]<-sum(qGtmp1[a,1:8,r,y])*N[r,1,y]
      }
    }

    for(a in 1:Nages){
      PopAge[a,y]<-sum(qGtmp2[a,1:Nrec,y])/Ntot[1,y]
    }
    for(l in 1:8){
      alphaG[1:Nages,l,y]<-Gstar[1:Nages,l,y]*etaG
      Gstar[1:Nages,l,y]~ddirich(aG)
      MG[1:Nages,l,y]<-log(Gstar[1:Nages,l,y])-0.5*(1/tauG[1:Nages,l,y])
      tauG[1:Nages,l,y]<-1/log((1/alphaG[1:Nages,l,y])+1)
    }
    for(s in 1:2){
      alphaL[1:8,s,y]<-Lstar[1:8,s,y]*(etaL[s]+1)
      Lstar[1:8,s,y]~ddirich(aL)
      ML[1:8,s,y]<-log(Lstar[1:8,s,y])-0.5*(1/tauL[1:8,s,y])
      tauL[1:8,s,y]<-1/log((1/alphaL[1:8,s,y])+1)
    }
  }

  for(s in 1:2){
    etaL[s]~dlnorm(0.8,0.1)#dlnorm(4.6,0.7)#dunif(1,10000)#dlnorm(4.6,0.7)
#    etaR[s]~dlnorm(0.8,0.1)#dlnorm(4.6,0.7)#~dunif(10,1000) # ajattele eta otoskokona joka jaetaan eri luokkiin dir-jakaumassa.
    # spatiaalisen vaihtelun m??r?, voitaisiin ehk? pit?? samana vuosien yli (ainakin alkuun)
    # mit? pienempi etaR, sit? v?hemm?n kalat jakautuneet ruutujen pinta-alan mukaan.
    # my?hemmin voitais tehd? t?m? niin ett? etaR riippuu kalojen m??r?st? -> v?h?n kalaa, suurempi keskittyminen samoille paikoille.
#    etaE[s]<-exp(etaEZ[s])
  # etaE voisi periaatteessa riippua kalojen m??r?st?, mutta pidet??n nyt samana yli vuosien
#    etaEZ[s]~dnorm(13,0.0000001)  # t?m? parametrisointi voi auttaa JAGSin kanssa
  for(y in 1:Nyears){
    etaE[y,s]~dlnorm(ME[s],tauE[s])
    etaR[y,s]~dlnorm(MR[s],tauR[s])
  }
  }
#  etaR~dlnorm(0.8,0.1)#dlnorm(4.6,0.7)#~dunif(10,1000) # ajattele eta otoskokona joka jaetaan eri luokkiin dir-jakaumassa.
  muE[1]~dunif(1,100000)
  cvE[1]~dunif(0.01,2)
  ME[1]<-log(muE[1])-0.5/tauE[1]
  tauE[1]<-1/(log(cvE[1]*cvE[1]+1))
  muR[1]~dunif(1,1000)
  cvR[1]~dunif(0.01,2)
  MR[1]<-log(muR[1])-0.5/tauR[1]
  tauR[1]<-1/(log(cvR[1]*cvR[1]+1))

muE[2]~dunif(1,100000)
  cvE[2]~dunif(0.01,2)
  ME[2]<-log(muE[2])-0.5/tauE[2]
  tauE[2]<-1/(log(cvE[2]*cvE[2]+1))
  muR[2]~dunif(1,1000)
  cvR[2]~dunif(0.01,2)
  MR[2]<-log(muR[2])-0.5/tauR[2]
  tauR[2]<-1/(log(cvR[2]*cvR[2]+1))
  

etaG~dlnorm(0.8,0.1)#dunif(1,10000)
  #etaH~dlnorm(0.8,0.1)
  etaH~dbeta(2,2)
  # meanL: midpoint of each length class
  #sigmaL[1:8]<-9.533*pow(10,-7)*pow(meanL[1:8],2)

  sigmaL[1:8]<-4*pi*pow(10,-TSa/10)*pow(meanL[1:8],2)
  TSa<-71.2
  #TSa~dlnorm(4.23,243)# mu=68.72, sd=4.415
  # Unupdated priors
  ##############################
  NTX<-exp(NtmpX)
  NtmpX~dnorm(13,0.0000001)
  cv_nascX~dlnorm(0.03,3.26)
  etaX~dlnorm(0.8,0.1)


}"

#cat(GRAHS,file="GRAHS.txt")

#############################


data<-list(
  Nyears=2,
  Nrec=4,
  Nages=10,
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
  aL=rep(1,8),
  meanL=meanL
)

parnames=c(
  "PopAge",
  "Lstar",
  "cv_nasc",
  "etaR", "etaE", "etaL","etaG","etaH",
  "Ntot","N"
)


run0<-run.jags(GRAHS_model1, monitor=parnames,data=data,n.chains = 2, method = 'parallel', thin=1,
         burnin =1000, modules = "mix",
         sample =1000, adapt = 1000,
         keep.jags.files=F,
         progress.bar=TRUE, jags.refresh=100)

t01<-Sys.time();print(t01)
run1<-run.jags(GRAHS_model1, monitor=parnames,data=data,n.chains = 2, method = 'parallel', thin=500,
               burnin =10000, modules = "mix",
               sample =2000, adapt = 15000,
               keep.jags.files=F,
               progress.bar=TRUE, jags.refresh=100)
save(run1, file="../BIAS_ms.RData")
t02<-Sys.time();print(t02)
print("run1 done");print(difftime(t02,t01))
print("--------------------------------------------------")



