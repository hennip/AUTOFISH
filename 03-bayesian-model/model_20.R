
modelname<-"GRAHS20"
GRAHS_model<-GRAHS20<-"
model{

  # Annual abundances
  # ===========================================================
  for(s in 1:Nspecies){
    for(y in 1:Nyears){
    for(r in 1:Nrec){
      N[s,r,y]<-exp(Ntmp[s,r,y])
      Ntmp[s,r,y]~dnorm(13,0.0000001)

    }
    Ntot[s,y]<-sum(N[s,1:Nrec,y])
    }
  }

  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      for(r in 1:Nrec){
        for(e in 1:Necho[r,y]){
          # n: number of fish of species s on echo area e of rectangle r
          n[e,s,r,y]<-N[s,r,y]*pE[e,s,r,y]
        }
        pE[1:Necho[r,y],s,r,y]~ddirich(alphaE[1:Necho[r,y],s,r,y])
        alphaE[1:Necho[r,y],s,r,y]<-propA[1:Necho[r,y],r,y]*N[s,r,y]*etaE[s]#etaE[s,r,y]
      }
    }
  }

# Observation model for echosound data
# ===========================================================
  for(i in 1:Nobs){# total number of observations over years
  
    #NASC[i]~dlnorm(M_nasc[i], tau_nasc[R[i],nascY[i]]) # NASC (m2/NM2)
    NASC[i]~dlnorm(M_nasc[i], tau_nasc) # NASC (m2/NM2)
  
    # Expected NASC at piece of cruise track i, year nascY[i] is a combination 
    # of sigmaR and n over 4 species divided by the area covered 
    mu_nasc[i]<- sum(sigmaR[1:4,R[i],nascY[i]]*n[LOG[i],1:4,R[i],nascY[i]])/
      (pA[i]*A[R[i]])
  
    #M_nasc[i]<-log(mu_nasc[i])-0.5*(1/tau_nasc[R[i],nascY[i]])
    M_nasc[i]<-log(mu_nasc[i])-0.5*(1/tau_nasc)
    propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
  }

  cv_nasc~dlnorm(0.03,3.26) # measurement error, same over years
  tau_nasc<-1/log(cv_nasc*cv_nasc+1)
  # for(y in 1:Nyears){
  #   for(r in 1:Nrec){
  #     tau_nasc[r,y]<-1/log(cv_nasc[r,y]*cv_nasc[r,y]+1)
  #     cv_nasc[r,y]~dunif(0.1,5)#dlnorm(0.03,3.26) # measurement error, same over years
  #   }
  # }

  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      for(r in 1:Nrec){
        sigmaR[s,r,y]<-sum(qL[1:Nlengths[s],s,r,y]*sigmaL[1:Nlengths[s],s])
      }}
      
    # meanL: midpoint of each length class
    sigmaL[1:Nlengths[s],s]<-4*pi*pow(10,TSa/10)*pow(meanL[1:Nlengths[s],s],2)
  }
  TSa<- -71.2

# Observation models for trawl data
# ===========================================================

  # Species composition (total trawl catch)
  # =======================================
  for(y in 1:Nyears){
    for(r in 1:Nrec){
      for(h in 1:Nhaul[r,y]){ # Several hauls per ruhne rectangle
        Sobs[1:Nspecies,h,r,y]~dmulti(qS[1:Nspecies,r,y],Cobs[h,r,y])
      }
      
       for(s in 1:Nspecies){
       qS[s,r,y]<-N[s,r,y]/sum(N[1:Nspecies,r,y])
       }
     }
  }
      
      
  # Length composition (catch sample)
  # =================================
  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      for(r in 1:Nrec){
        for(h in 1:Nhaul[r,y]){
          # Observed number of fish of species s in each length class in rectangle r
          # in haul h
          Lobs[1:Nlengths[s],h,s,r,y]~dmulti(qL[1:Nlengths[s],s,r,y],nLobs[h,s,r,y])
        }
        # approximate dirichlet (set of gamma distributions) with lognormal distns
        qL[1:Nlengths[s],s,r,y]<-zL[1:Nlengths[s],s,r,y]/sum(zL[1:Nlengths[s],s,r,y])

        for(l in 1:Nlengths[s]){
          zL[l,s,r,y]~dlnorm(ML[l,s,y],tauL[l,s,y])
        }
      }
    }
  }

  for(y in 1:Nyears){
    muL[1:Nlengths[1],1,y]~ddirich(aL1) # Herring
    muL[1:Nlengths[2],2,y]~ddirich(aL2) # Sprat
    muL[1:Nlengths[3],3,y]~ddirich(aL3) # GTA
    muL[1:Nlengths[4],4,y]~ddirich(aL4) # Other
    
    for(s in 1:Nspecies){
      ML[1:Nlengths[s],s,y]<-log(muL[1:Nlengths[s],s,y])-
                              0.5*(1/tauL[1:Nlengths[s],s,y])
      alphaL[1:Nlengths[s],s,y]<-muL[1:Nlengths[s],s,y]*(etaL[s]+1)
      tauL[1:Nlengths[s],s,y]<-1/log((1/alphaL[1:Nlengths[s],s,y])+1)
    }
  }
  

  # # Age composition of herring (aged individuals)
  # # =============================================
  # for(y in 1:Nyears){
  #   for(r in 1:Nrec){
  #     for(l in 1:Nlengths[1]){ # Age data on herring only
  #       # Gobs: observed number of herring of each age class in length class l
  #       Gobs[1:Nages,l,r,y]~dmulti(qG[1:Nages,l,r,y],nGobs[l,r,y])
  #       # qG: age distribution of length class l
  # 
  #       #qG~ddirich(alphaG[1:Nages,l,y]) but
  #       # approximate dirichlet (set of gamma distributions) with lognormal distns
  #       qG[1:Nages,l,r,y]<-zG[1:Nages,l,r,y]/sum(zG[1:Nages,l,r,y])
  # 
  #       for(a in 1:Nages){
  #         zG[a,l,r,y]~dlnorm(MG[a,l,y],tauG[a,l,y])
  #         p_at_age[a,l,r,y]<-qL[l,1,r,y]*qG[a,l,r,y] # Proportion of herring at age on length
  #       }
  #     }
  #     for(a in 1:Nages){
  #       n_at_age[a,r,y]<-sum(p_at_age[a,1:Nlengths[1],r,y])*N[1,r,y] # Number of herring at age
  #     }
  #   }
  # 
  #   for(a in 1:Nages){
  #     ageH[a,y]<-sum(nH_at_age[a,1:Nrec,y])/Ntot[1,y]
  #   }
  #   for(l in 1:Nlengths[1]){
  #     muG[1:Nages,l,y]~ddirich(aG)
  #     MG[1:Nages,l,y]<-log(muG[1:Nages,l,y])-0.5*(1/tauG[1:Nages,l,y])
  #     alphaG[1:Nages,l,y]<-muG[1:Nages,l,y]*etaG
  #     tauG[1:Nages,l,y]<-1/log((1/alphaG[1:Nages,l,y])+1)
  #   }
  # }
  # 
  # Dispersion parameters
  # ===========================================================
  
  etaG~dunif(0.0001,1000)  # Age composition of herring among catch samples

  for(y in 1:Nyears){
  for(r in 1:Nrec){
    etaS[r,y]~dunif(0.0001,1000)  # Species composition among trawl catches
  }}

  for(s in 1:Nspecies){
    etaL[s]~dunif(0.0001,1000)# Length composition per species among hauls  
    etaR[s]~dunif(0.001,1000)    # Spatial overdispersion between rectangles
    etaE[s]~dunif(0.001,1)    # Spatial overdispersion within rectangles
    # for(y in 1:Nyears){
    #   for(r in 1:Nrec){
    #     etaE[s,r,y]~dunif(0.001,1)    # Spatial overdispersion within rectangles
    #   }
    # }
  }


  # Unupdated priors
  # ===========================================================
  NTX<-NtmpX*1000000
  NtmpX~dunif(0.0001,100000)
  cv_nascX~dunif(0.1,5)#dlnorm(0.03,3.26)
  etaX1~dunif(0.0001,1000)
  etaX2~dunif(0.0001,1)


}"

cat(GRAHS_model,file=paste0(modelname,".txt"))
