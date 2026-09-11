
modelname<-"GRAHS4_NASC1"
GRAHS_model<-GRAHS4_NASC1<-"
model{

  # Annual abundances
  # ===========================================================
  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      Ntot[s,y]<-exp(Ntmp[s,y])
      Ntmp[s,y]~dnorm(13,0.0000001)
    }}

  # Spatial distribution
  # ===========================================================
  for(y in 1:Nyears){
    for(s in 1:Nspecies){
      for(r in 1:Nrec){
        # N: Number of fish of species s on rectangle r
        N[r,s,y]<-Ntot[s,y]*pR[r,s,y]
      }
      pR[1:Nrec,s,y]~ddirich(alphaR[1:Nrec,s,y])
      
      # Expected value is A[1:Nrec]/Atot, dispersion is Ntot[s,y]*etaR[s]
      alphaR[1:Nrec,s,y]<-(A[1:Nrec]/Atot)*Ntot[s,y]*etaR[s]

      for(r in 1:Nrec){
        for(e in 1:Necho[r,y]){
          # n: number of fish of species s on echo area e of rectangle r
          n[e,r,s,y]<-N[r,s,y]*pE[e,r,s,y]
        }
        pE[1:Necho[r,y],r,s,y]~ddirich(alphaE[1:Necho[r,y],r,s,y])
        alphaE[1:Necho[r,y],r,s,y]<-propA[1:Necho[r,y],r,y]*N[r,s,y]*etaE[r,s,y]
      }
    }
  }

# Observation model for echosound data
# ===========================================================
  for(i in 1:Nobs){# total number of observations over years
  
    NASC[i]~dlnorm(M_nasc[i], tau_nasc[R[i],nascY[i]]) # NASC (m2/NM2)
  
    # Expected NASC at piece of cruise track i, year nascY[i] is a combination 
    # of sigmaR and n over 4 species divided by the area covered 
    mu_nasc[i]<- sum(sigmaR[R[i],1:4,nascY[i]]*n[LOG[i],R[i],1:4,nascY[i]])/
      (pA[i]*A[R[i]])
  
    M_nasc[i]<-log(mu_nasc[i])-0.5*(1/tau_nasc[R[i],nascY[i]])
    propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
  }

  for(y in 1:Nyears){
    for(r in 1:Nrec){
      tau_nasc[r,y]<-1/log(cv_nasc[r,y]*cv_nasc[r,y]+1)
      cv_nasc[r,y]~dunif(0.1,5)#dlnorm(0.03,3.26) # measurement error, same over years
    }
  }

  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      for(r in 1:Nrec){
        sigmaR[r,s,y]<-sum(qL[1:Nlengths[s],r,s,y]*sigmaL[1:Nlengths[s],s])
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
        Sobs[1:Nspecies,h,r,y]~dmulti(qS[1:Nspecies,h,r,y],Cobs[h,r,y])

        # qS~ddirich() but  
        # approximate dirichlet (set of gamma distributions) with lognormal distns
        qS[1:Nspecies,h,r,y]<-zS[1:Nspecies,h,r,y]/sum(zS[1:Nspecies,h,r,y])

        for(s in 1:Nspecies){
          zS[s,h,r,y]~dlnorm(MS[s,r,y],tauS[s,r,y])
        }
      }
      for(s in 1:Nspecies){
        muS[s,r,y]<-N[r,s,y]/sum(N[r,1:Nspecies,y])
      }
      MS[1:Nspecies,r,y]<-log(muS[1:Nspecies,r,y])-0.5*(1/tauS[1:Nspecies,r,y])
      alphaS[1:Nspecies,r,y]<-muS[1:Nspecies,r,y]*(etaS[r,y]+1)
# KOITA TOIMIIKO JOS +1:n JÄTTÄÄ POIS TAI KORVAA JOLLAIN TOSI PIENELLÄ

      tauS[1:Nspecies,r,y]<-1/log((1/alphaS[1:Nspecies,r,y])+1)
      
    }}
      
      
  # Length composition (catch sample)
  # =================================
  for(s in 1:Nspecies){
  for(y in 1:Nyears){
    for(r in 1:Nrec){
        # Observed number of fish of species s in each length class in rectangle r
        Lobs[1:Nlengths[s],r,s,y]~dmulti(qL[1:Nlengths[s],r,s,y],nLobs[r,s,y])

        # approximate dirichlet (set of gamma distributions) with lognormal distns
        qL[1:Nlengths[s],r,s,y]<-zL[1:Nlengths[s],r,s,y]/sum(zL[1:Nlengths[s],r,s,y])

        for(l in 1:Nlengths[s]){
          zL[l,r,s,y]~dlnorm(ML[l,s,y],tauL[l,s,y])
        }
        sigmaR[r,s,y]<-sum(qL[1:Nlengths[s],r,s,y]*sigmaL[1:Nlengths[s],s])
      }}
      
    # meanL: midpoint of each length class
    sigmaL[1:Nlengths[s],s]<-4*pi*pow(10,TSa/10)*pow(meanL[1:Nlengths[s],s],2)
  }
  TSa<- -71.2
  
  for(y in 1:Nyears){
    muL[1:Nlengths[1],1,y]~ddirich(aL1)
    muL[1:Nlengths[2],2,y]~ddirich(aL2)
    muL[1:Nlengths[3],3,y]~ddirich(aL3)
    muL[1:Nlengths[4],4,y]~ddirich(aL4)
    
    for(s in 1:Nspecies){
      ML[1:Nlengths[s],s,y]<-log(muL[1:Nlengths[s],s,y])-0.5*(1/tauL[1:Nlengths[s],s,y])
      alphaL[1:Nlengths[s],s,y]<-muL[1:Nlengths[s],s,y]*(etaL[s]+1)
      tauL[1:Nlengths[s],s,y]<-1/log((1/alphaL[1:Nlengths[s],s,y])+1)
    }
    
  }

  # Age composition of herring (aged individuals)
  # =============================================
  for(y in 1:Nyears){
    for(r in 1:Nrec){
      for(l in 1:Nlengths[1]){ # Age data on herring only
        # Gobs: observed number of herring of each age class in length class l
        Gobs[1:Nages,l,r,y]~dmulti(qG[1:Nages,l,r,y],nGobs[l,r,y])
        # qG: age distribution of length class l

        #qG~ddirich(alphaG[1:Nages,l,y]) but
        # approximate dirichlet (set of gamma distributions) with lognormal distns
        qG[1:Nages,l,r,y]<-zG[1:Nages,l,r,y]/sum(zG[1:Nages,l,r,y]) 
        
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
      ageH[a,y]<-sum(nH_at_age[a,1:Nrec,y])/Ntot[1,y]
    }
    for(l in 1:Nlengths[1]){
      muG[1:Nages,l,y]~ddirich(aG)
      MG[1:Nages,l,y]<-log(muG[1:Nages,l,y])-0.5*(1/tauG[1:Nages,l,y])
      alphaG[1:Nages,l,y]<-muG[1:Nages,l,y]*etaG
      tauG[1:Nages,l,y]<-1/log((1/alphaG[1:Nages,l,y])+1)
    }
  }
  
  # Dispersion parameters
  # ===========================================================
  
  etaG~dunif(0.0001,1000)  # Age composition of herring among catch samples

  for(y in 1:Nyears){
  for(r in 1:4){
    etaS[r,y]~dunif(0.0001,1000)  # Species composition among trawl catches
  }
  }

  for(s in 1:Nspecies){
    etaL[s]~dunif(0.0001,1000)# Length composition per species among hauls  
    etaR[s]~dunif(0.001,1)    # Spatial overdispersion between rectangles
    #etaE[s]~dunif(0.001,1)    # Spatial overdispersion within rectangles
  }

for(r in 1:4){
  for(y in 1:Nyears){
    # Trial: assume that schooling can take place out of chance in any rec-year
    # combination for herring, sprat or gta and that we can't know when and where such happens
    # Also a rectangle can be empty of one species as well
    # Let etaE adjust per case, later hierarchical structure could be assumed instead
  for(s in 1:(Nspecies-1)){ # Herring, sprat & gta
    etaE[r,s,y]~dunif(0.001,1)    # Spatial overdispersion within rectangles

    # Sitä paitsi, ei ole kyse edes siitä että troolisaaliin vaihtelu kertoisi jotain pelkästään
    # lajin parvikäytöksestä, vaan myös siitä miten päätös siitä missä ja milloin troolataan, tehdään!!!!

  }
  etaE[r,4,y]<-etaE4
  }
}
# Other species, assume the overdispersion the same always
etaE4~dunif(0.001,1)

  # Unupdated priors
  # ===========================================================
  NTX<-exp(NtmpX)
  NtmpX~dnorm(13,0.0000001)
  cv_nascX~dunif(0.1,5)#dlnorm(0.03,3.26)
  etaX1~dunif(0.0001,1000)
  etaX2~dunif(0.0001,1)


}"

cat(GRAHS_model,file=paste0(modelname,".txt"))
