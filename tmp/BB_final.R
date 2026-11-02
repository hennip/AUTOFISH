             
dbetabin<-nimbleFunction(run=function(x=double(0),mu=double(0),phi=double(0),size=double(0), log = integer(0, default=0)){
  returnType(double(0))

  #if(x>=0&x<=size){
  logProb<-lgamma((size+0.001)+1)+lgamma(x+mu*phi)+lgamma((size+0.001)-x+(1-mu)*phi)+lgamma(phi)-
             lgamma((size+0.001)+phi)-lgamma(mu*phi)-lgamma((1-mu)*phi)-lgamma((size+0.001)-x+1)-lgamma(x+1)
  if (log) return(logProb)
  else return(exp(logProb))
#},buildDerivs = TRUE)                                                                                           
})

rbetabin<-nimbleFunction(run=function(n=integer(0),mu=double(0),phi=double(0),size=double(0)){
  pr<-rbeta(1,mu*phi,(1-mu)*phi)
  returnType(double(0))
  return(rbinom(1,round(size),pr))
})

#phi=eta or (1/corr)-1 
# Register the Beta-Binomial as a distribution for NIMBLE (see Functions.R for dbetabin).
registerDistributions(list(dbetabin=list(
  BUGSdist='dbetabin(mu,phi,size)')))      #,discrete=TRUE)

#########################################################################################   
smoltCode<-nimbleCode({ 
## Prior distribution for the population size ##

  U <- round(CU)                                              
  CU <- (exp(logU))                                          
  logU ~ dunif(5, 15.50)  #upper bound from 14.5 to 15.5 October 2026  

  for(j in 1:N) {
    qP[j] <- qmu[j] * P[j]
    delts[j] <- 1/((sigma_obs + 1) * (1 -  qP[j]) / (P[j] * (1 - qmu[j])) - 1)   #correlation (high when sigma is low, used as parameter of Samu's BB)
    eta.c[j]<-1/delts[j]-1     #back to sigma for catch
    Ncatch[j]~dbetabin(qP[j],eta.c[j],CU)     #mu, phi, size
    cx[j]~dbetabin(qP[j],eta.c[j],CU)     #mu, phi, size

  }
  
  for(i in 1:(N-1)) {
   
  #theta[i,(N+1)]<-1-sum(theta[i,(i+1):N]) 
   for(j in (i + 1):N) {	
         np[i, j] <- theta[i, j]*m[i]+0.0000001
         pr[i, j] <- theta[i, j]*qmu[i]+0.0000001
   }
  }
  
  theta[N,N]<-1
  
#  P[1:N]~ddirch(alphaP[1:N])
  
for(i in 1:N) {              
    
    g[i] ~ T(dgamma(ag[i, w], 1),0.001, )
   P[i] <- g[i] / sum(g[1:N])
    
  
    ag[i, 1] <- 0.029
    ag[i, 2] <- 0.1
    ag[i, 3] <- 0.1

## Logit-normal model for the catchability, see eqn. 4 ##
        
    logit(qmu[i])  <-  eta[i]                             
    eta[i] ~ dnorm(etamu[i], invxi2)	
	  etamu[i] <- nu0 + nu1 * swl[i] + nu2 * swt[i]	

    for(j in (i + 1):N) {					
       theta[i, j]<-(phi(((log(j-i+0.5)-mu[i]))/lsigma[i])-step(j-i-1.1)*phi(((log(j-i-0.5)-mu[i]))/lsigma[i]))
       
       theta1[i, j]<-(phi(((log(j-i+0.5)-mu[i]))/lsigma[i])-step(j-i-1.1)*phi(((log(j-i-0.5)-mu[i]))/lsigma[i]))/((phi(((log(N-i+0.5)-mu[i]))/lsigma[i])-phi((log(0.5)-mu[i])/lsigma[i]))+0.0000001) 
       check[i,j]<-((phi(((log(N-i+0.5)-mu[i]))/lsigma[i])-phi((log(0.5)-mu[i])/lsigma[i]))+0.0000001) 
       
   #    pmov[i,j,s]<-(phi((log(IN[j+1])-(log(14)+mean_v))/sigma_v)-phi((log(IN[j])-(log(14)+mean_v))/sigma_v))/(phi((log(IN[ubound_s[i,s]+1])-(log(14)     #                                  +mean_v))/sigma_v)-phi((log(IN[1])-(log(14)+mean_v))/sigma_v))  
       
    }

cv[i]<-phi1[i]/lambda[i]	
lsigma[i]<-sqrt(log(cv[i]*cv[i]+1))
tau[i]<-1/log(cv[i]*cv[i]+1)
mu[i]<-log(lambda[i])-0.5/tau[i]   

# Regression models for the movement parameters, see eqns. 5 and 6 ##

    phi1[i] <- exp(lphi[i])+0.0000001
    lphi[i] ~ dnorm(gamma[i], invrho2)
    gamma[i] <- omega0 + omega1 * swl[i] + omega2 * swt[i]	

    lambda[i] <- exp(llambda[i])
    llambda[i] ~ dnorm(delta[i], invpi2)
    delta[i] <- psi0 + psi1 * swl[i] + psi2 * swt[i]
} #i loop



for(i in 1:nrobs){
    for(j in (rind[i] + 1):N) {			
  
    r[rind[i], j]~dbetabin(pr[rind[i], j],sigma_obs,m[rind[i]])     #mu, phi, size
    rx[rind[i], j]~dbetabin(pr[rind[i], j],sigma_obs,m[rind[i]])
  }
}

## Prior distributions for hyperparameters ##
			
nu0 ~ dnorm(0, 1)
nu1 ~ dnorm(0, 2)
nu2 ~ dnorm(0, 2)

sigma_obs ~ T(dgamma(0.01, 0.01),0.01,)  #orig dgamma(0.001,0.001), doesn't work well in Nimble
#sigma_obs ~ dgamma(0.01, 0.01)
#sigma_obs ~ dunif(10, 1000)
xi~dlnorm(-0.69,2) #sd                 
invxi2 <- 1 / pow(xi, 2)

rho~dlnorm(-0.69,2)
invrho2 <- 1 / pow(rho, 2)

pi~dlnorm(-0.69,2)
invpi2 <- 1 / pow(pi, 2)

omega0 ~ dnorm(0, 1)    #0.001        
omega1 ~ dnorm(0, 1)
omega2 ~ dnorm(0, 1)		

psi0 ~ dnorm(0, 1)
psi1 ~ dnorm(0, 1)
psi2 ~ dnorm(0, 1)	

w ~ dcat(wp[1:3])

wp[1] <- 1 / 3
wp[2] <- 1 / 3
wp[3] <- 1 / 3

})   #Nimble

assign('dbetabin', dbetabin, envir = .GlobalEnv)
assign('rbetabin', rbetabin, envir = .GlobalEnv)
