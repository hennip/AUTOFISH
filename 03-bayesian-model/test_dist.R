
testM<-"
model{


  for(s in 1:Nspecies){
    #etaS[s]~dlnorm(0.8,0.1)
    etaS[s,y]~dlnorm(log(mu_etaS[s])-0.5*log(pow(cv_etaS,2)+1), 1/log(pow(cv_etaS,2)+1))
    
    etaR[s]~dlnorm(0.8,0.1)
    etaE[s]<-exp(etaEZ[s])
    etaEZ[s]~dnorm(13,0.0000001)  # this parameterisation may help with JAGS
    #etaEZ[s]~dnorm(mu_EZ,1/pow(sd_EZ,2))
    etaL[s]~dlnorm(0.8,0.1)
  }
  mu_etaS[s]~dlnorm(log(10)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
  cv_etaS~dunif(0.01,2)
  
  mu_EZ~dnorm(13,0.001)
  sd_EZ~dlnorm(log(1000)-0.5*log(1*1+1),log(1*1+1))# tau=1/log(cv^2+1)


}"
modelname<-"testM"


parnames=c(
  #"muH",
  "muS",
  "PopAge",
  "Lstar",
  "cv_nasc", "cv_nascX", "etaX",
  "etaR", "etaE", "etaL","etaG","etaS",#"etaH", 
  "Ntot","N"
)

# 
run0<-run.jags(GRAHS_model4, monitor=parnames,data=data,n.chains = 2, method = 'parallel', thin=100,
               burnin =1000, modules = "mix",
               sample =1000, adapt = 1000,
               keep.jags.files=F,
               progress.bar=TRUE, jags.refresh=100)
plot(run0, var="etaE")


