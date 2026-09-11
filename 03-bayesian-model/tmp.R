x~dbin(N,p)
p~(mu*eta, (1-mu)*eta)
x_pred~dbetabin(mu*eta, (1-mu)*eta)

x~dmulti(p,N)
p~ddirich(alpha)
alpha<-mu*eta

# mu on keskimääräinen osuus, mutta aloha sisältää myös hajonnan

# alphaS
# alphaL


"
# Observation model for echosound data
# ===========================================================
for(i in 1:Nobs){# total number of observations over years
  
  NASC[i]~dlnorm(M_nasc[i,nascY[i]], tau_nasc) # NASC (m2/NM2)
  
  # Expected NASC at piece of cruise track i, year nascY[i] is a combination 
  # of sigmaR and n over 4 species divided by the area covered 
  mu_nasc[i,nascY[i]]<- sum(sigmaR[R[i],1:4,nascY[i]]*n[LOG[i],R[i],1:4,nascY[i]])/
    (pA[i]*A[R[i]])
  
  M_nasc[i,nascY[i]]<-log(mu_nasc[i,nascY[i]])-0.5*(1/tau_nasc)
  propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
}
tau_nasc<-1/log(cv_nasc*cv_nasc+1)
cv_nasc~dlnorm(0.03,3.26) # measurement error, same over years
"

# what is rectangle specific mean sigma? Is it the same as in excel?
# Why we don't need it or do we need it
# -> we need it at least for comparison purposes

"

# Observation model for echosound data
# ===========================================================
# Alternative 1: Remove nascY from M_nasc[i,nascY[i]] as it doesn't seem to do anything there
# make cv_nasc to vary per year and rec since maybe we don't have a reason to believe the variation couldn't
# be different?
# monitor mu_nasc and NASC for areas not visited -> that should be the rectangle specific SA
# these two shouldn't differ from each other too much I think, hard to say
# -> anyways can be compared to the figures Elor has


for(i in 1:Nobs){# total number of observations over years
  
  NASC[i]~dlnorm(M_nasc[i], tau_nasc[R[i],nascY[i]]) # NASC (m2/NM2)
  
  # Expected NASC at piece of cruise track i, year nascY[i] is a combination 
  # of sigmaR and n over 4 species divided by the area covered 
  mu_nasc[i]<- sum(sigmaR[R[i],1:4,nascY[i]]*n[LOG[i],R[i],1:4,nascY[i]])/
    (pA[i]*A[R[i]])
  
  M_nasc[i,nascY[i]]<-log(mu_nasc[i,nascY[i]])-0.5*(1/tau_nasc)
  propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
}

for(y in 1:Nyears){
for(r in 1:Nrec){
tau_nasc[r,y]<-1/log(cv_nasc[r,y]*cv_nasc[r,y]+1)
cv_nasc[r,y]~dlnorm(0.03,3.26) # measurement error, same over years
}
}

"

"

# Observation model for echosound data
# ===========================================================
# Alternative 2: 

for(i in 1:Nobs){# total number of observations over years
  
  NASC[i]~dlnorm(M_nasc[i,R[i],nascY[i]], tau_nasc[R[i],nascY[i]]) # NASC (m2/NM2)
  
  # Expected NASC at piece of cruise track i, year nascY[i] is a combination 
  # of sigmaR and n over 4 species divided by the area covered 
  mu_nasc[i]<- sum(sigmaR[R[i],1:4,nascY[i]]*n[LOG[i],R[i],1:4,nascY[i]])/
    (pA[i]*A[R[i]])
  
  M_nasc[i,nascY[i]]<-log(mu_nasc[i,nascY[i]])-0.5*(1/tau_nasc)
  propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
}

for(y in 1:Nyears){
for(r in 1:Nrec){
tau_nasc[r,y]<-1/log(cv_nasc[r,y]*cv_nasc[r,y]+1)
cv_nasc[r,y]~dlnorm(0.03,3.26) # measurement error, same over years
}
}

"


"
# Observation model for echosound data
# ===========================================================
# Alternative 2: Calculate the bloody mean nasc per rectangle and use that only
# Anyways the observed area is so tiny it shouldn't make much difference
# On the other hand would this somehow ruin the purpose of the model..???
# Maybe it was just a mistake in thinking that this was missing in the first place?
# Holy cow this probably means the whole data must be arranged differently...

# Koko homma riippuu varmaan siitä mitä cv_nasc ylipäänsä tarkoittaa. Mittausvirhe
# jonka arvo on cv:nä luokkaa 2, eli valtava!!!


for(i in 1:Nobs){# total number of observations over years
  
  NASC[i,r,y]?
  NASC[i]~dlnorm(M_nasc[i,R[i],nascY[i]], tau_nasc) # NASC (m2/NM2)
  
  # Expected NASC at piece of cruise track i, year nascY[i] is a combination 
  # of sigmaR and n over 4 species divided by the area covered 
  mu_nasc[R[i],nascY[i]]<- sum(sigmaR[R[i],1:4,nascY[i]]*n[LOG[i],R[i],1:4,nascY[i]])/
    (pA[i]*A[R[i]])
  
  M_nasc[i,nascY[i]]<-log(mu_nasc[i,nascY[i]])-0.5*(1/tau_nasc)
  propA[LOG[i],R[i],nascY[i]]<-pA[i] # proportion of area i of rectangle R[i]
}
tau_nasc<-1/log(cv_nasc*cv_nasc+1)
cv_nasc~dlnorm(0.03,3.26) # measurement error, same over years
"

# Onko ongelma tässä? NASC-arvojen avulla ei päästä ennustamaan keskimääräistä
# NASCia (ei ainakaan suoraan), sillä odotusarvo voi riippua joko muista havainnoista (samalla ruudulla, samana vuonna)
# TAI malliennusteesta!
# Tässä mielessä keskimääräistä NASCia ei ole olemassakaan, eikä sille ole paikkaa mallissa