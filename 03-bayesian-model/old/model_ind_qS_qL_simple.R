
modelname<-"GRAHS4_ind_qS_qL_simple"
GRAHS_model<-GRAHS4_ind_qS_qL_simple<-"
model{

  # Annual abundances
  # ===========================================================
  for(s in 1:Nspecies){
    for(y in 1:Nyears){
      Ntot[s,y]<-Ntmp[s,y]*1000000
      Ntmp[s,y]~dunif(0.0001,100000)
    }}

  # Spatial distribution
  # ===========================================================
  for(y in 1:Nyears){
    for(s in 1:Nspecies){
      for(r in 1:Nrec){
        # N: Number of fish of species s on rectangle r
        N[s,r,y]<-Ntot[s,y]*pR[s,r,y]
      }
      pR[s,1:Nrec,y]~ddirich(alphaR[s,1:Nrec,y])
      
      # Expected value is A[1:Nrec]/Atot, dispersion is Ntot[s,y]*etaR[s]
      alphaR[s,1:Nrec,y]<-(A[1:Nrec]/Atot)*Ntot[s,y]*etaR[s]

      for(r in 1:Nrec){
        for(e in 1:Necho[r,y]){
          # n: number of fish of species s on echo area e of rectangle r
          n[e,s,r,y]<-N[s,r,y]*pE[e,s,r,y]
        }
        pE[1:Necho[r,y],s,r,y]~ddirich(alphaE[1:Necho[r,y],s,r,y])
        alphaE[1:Necho[r,y],s,r,y]<-propA[1:Necho[r,y],r,y]*N[s,r,y]*etaE[s]
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

  # Species composition
  # =======================================
  for(y in 1:Nyears){
    for(r in 1:Nrec){
      for(h in 1:Nhaul[r,y]){ 
        # Observed species composition in haul h sample
        Sobs[1:Nspecies,h,r,y]~dmulti(qS[1:Nspecies,r,y],Cobs[h,r,y])
      }
      for(s in 1:Nspecies){
        qS[s,r,y]<-N[s,r,y]/sum(N[1:Nspecies,r,y])
      }
    }
  }
      
      
  # Length composition
  # =================================
  for(y in 1:Nyears){
    for(r in 1:Nrec){
      for(s in 1:Nspecies){
        for(h in 1:Nhaul[r,y]){
          # Observed length composition of species s in haul h sample
          Lobs[1:Nlengths[s],h,s,r,y]~dmulti(qL[1:Nlengths[s],s,r,y],nLobs[h,s,r,y])
        }
      }
      
      qL[1:Nlengths[1],1,r,y]~ddirich(aL1) # Herring
      qL[1:Nlengths[2],2,r,y]~ddirich(aL2) # Sprat
      qL[1:Nlengths[3],3,r,y]~ddirich(aL3) # GTA
      qL[1:Nlengths[4],4,r,y]~ddirich(aL4) # Other
    }
  }



  # Age composition of herring
  # ===========================================================
  for(y in 1:Nyears){
    for(r in 1:Nrec){
      for(l in 1:Nlengths[1]){ # Age data on herring only
        # Gobs: observed age composition in length class l
        Gobs[1:Nages,l,r,y]~dmulti(qG[1:Nages,l,r,y],nGobs[l,r,y])
  
        # We assume dirichlet-multinomial instead of just multinomial if
        # we think that the growth is similar in different areas (recs)
        # but we don't expect it to be the same (instead assume those are exchangeable)
        # Hierarchical structure is more messy but here it makes more sense
        # than with the species or length composition
        
        # qG~ddirich(alphaG[1:Nages,l,y]) but
        # approximate dirichlet (set of gamma distributions) with lognormal distns
        qG[1:Nages,l,r,y]<-zG[1:Nages,l,r,y]/sum(zG[1:Nages,l,r,y]) 
        
        for(a in 1:Nages){
          zG[a,l,r,y]~dlnorm(MG[a,l,y],tauG[a,l,y])
          pH_at_age[a,l,r,y]<-qL[l,1,r,y]*qG[a,l,r,y] # Proportion of herring at age on length
        }
      }
      for(a in 1:Nages){
        nH_at_age[a,r,y]<-sum(pH_at_age[a,1:Nlengths[1],r,y])*N[1,r,y] # Number of herring at age
      }
    }

    for(a in 1:Nages){
      ageH[a,y]<-sum(nH_at_age[a,1:Nrec,y])/Ntot[1,y]
    }
    for(l in 1:Nlengths[1]){
      muG[1:Nages,l,y]~ddirich(aG)
      alphaG[1:Nages,l,y]<-muG[1:Nages,l,y]*etaG
      MG[1:Nages,l,y]<-log(muG[1:Nages,l,y])-0.5*(1/tauG[1:Nages,l,y])
      tauG[1:Nages,l,y]<-1/log((1/alphaG[1:Nages,l,y])+1) 
    }
  }
  
  # Dispersion parameters
  # ===========================================================
  
  etaG~dunif(0.0001,1000)  # Age composition of herring among catch samples

  for(y in 1:Nyears){
    etaS[y]~dunif(0.0001,1000)  # Species composition among trawl catches
  }

  for(s in 1:Nspecies){
    etaL[s]~dunif(0.0001,1000)# Length composition per species among hauls  
    etaR[s]~dunif(0.001,1)    # Spatial overdispersion between rectangles
    etaE[s]~dunif(0.001,1000)    # Spatial overdispersion within rectangles
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
