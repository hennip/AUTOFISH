

#setwd("//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/Benchmark_2026/Smolt_mark_recap/")
library(coda)
library(extraDistr)
library(nimble)
library(parallel)
library(readxl)
library(tidyverse)
library(lubridate)

source("plotfunctions.r")


modelfile<-"BB_final.r"
source(modelfile)





#df_catch<-read_xls(str_c(path_in,"Smolttisaalis_2025_AR.xls"), 
                    
df_catch<-read_xls("Smolttisaalis_2025_AR.xls", 
              sheet="saalis", col_names=T, range="A4:I40") |> 
  rename(w_temp=`veden l?mp?tila/ water temperature`,
         w_height=`vedenkorkeus/ water level`) |> 
  mutate(date_yday=yday(pvm),catch=lohi) |> 
  select(date_yday, everything()) |> 
  complete(date_yday) # Fills NA if a date is missed

#df_recaps<-read_xlsx(str_c(path_in,"Merkinn?t_2025.xlsx"), 
df_recaps<-read_xlsx("Merkinn?t_2025.xlsx",
                    sheet="Yksil?data lohi", col_names=T, guess_max = 5000 )|> 
  rename(rel_date=`vapautus "p?iv?"`, recap_date=`Takaisin-saanti-"p?iv?"`)|> 
  mutate(rel_yday=yday(rel_date),
         recap_yday=yday(recap_date))

# Check that start dates are the same
min(df_catch$date_yday)
min(df_recaps$rel_yday, na.rm=T)

# Covariates
wt<-df_catch$w_temp
wl<-df_catch$w_height

# m: merkittyjen m??r? (marked)
# c: rys?saalis (catch)
df_c<-df_catch |> select(date_yday,catch) |> rename(date=date_yday)

df_m<-df_recaps |> group_by(rel_yday) |> rename(date=rel_yday) |> 
  summarise(m=n())

df_mc<-full_join(df_c, df_m) |> 
  filter(is.na(date)==F)
N<-dim(df_mc)[1]

m<-df_mc |> select(m) |> pull()
m[is.na(m)]<-0
rind<-which(m!=0)  #indices with non 0 releases of tagged fish 
rind<-rind[!(rind %in% N)] 


# Calculate r matrix:
df<-  df_recaps |> 
  group_by(rel_yday, recap_yday) |> 
  summarise(n=n()) |> ungroup() |> 
  arrange(rel_yday,recap_yday) |> 
  filter(is.na(rel_yday)==F)|> 
  filter(is.na(recap_yday)==F)

min_d<-min(df$rel_yday, na.rm = T)
min(df_recaps$rel_date, na.rm=T)
max_d<-max(df$recap_yday, na.rm = T)

c_empty<-seq(min_d, max_d, by =1) 
df_r<-array(0, dim=c(length(c_empty),length(c_empty)))
for(i in 1:dim(df)[1]){
  tmp_reld<-df$rel_yday[i]-min_d+1
  tmp_recd<-df$recap_yday[i]-min_d+1
  df_r[tmp_reld,tmp_recd]<-df$n[i]
}
#View(df_r)


# For checkup    
df_recaps$rel_date
x<-df_recaps|> filter(rel_yday==153, is.na(recap_date)==F) |> 
  select(rel_yday, recap_yday, everything())
print(x=x, n=100)


alphaP<-rep(1/N,times=N)+0.1
make.inits <- function(){list(sigma_obs = runif(1,50,100),
               nu0 = runif(1,0.001,0.02),
               nu1 = rnorm(1,0,0.20),
               nu2 = rnorm(1,0,0.20),
               omega0 = rnorm(1,0,1),
               omega1 = rnorm(1,0,1),
               omega2 = rnorm(1,0,1),
               psi1 = rnorm(1,0,0.1),
               psi2 = rnorm(1,0,0.1),
               psi0 = rnorm(1,0,0.1),
               pi = rlnorm(1,-0.8,0.20),
               xi = rlnorm(1,-0.69,0.20),
               rho = rlnorm(1,-0.69,0.20),
               logU = runif(1,5.5,14),
               eta=runif(N,-3,3),
               llambda = runif(N,-10,1),
               lphi = runif(N,-10,1),   
               g=rep(0.1,times=N),
               P=as.numeric(rdirichlet(1,alphaP)))}  
               
Mconsts<-list(N=N, rind=rind,nrobs=length(rind),alphaP=rep(1/N,times=N)+0.1)
Mdata<-list(m=as.vector(m) ,  
            swt=(wt[1:N]-mean(wt))/sd(wt), 
            swl=(wl[1:N]-mean(wl))/sd(wl),
            Ncatch=df_mc$catch,r=df_r)  

#Mconsts<-list(N=N, rind=rind,nrobs=length(rind),mu_mu_ag=log(N/2))
#Mdata<-list(m=m ,  swt=(wt[1:N]-mean(wt))/sd(wt), swl=(wl[1:N]-mean(wl))/sd(wl),Ncatch=catch,r=r)  
              
parnames<-c("P","omega0","omega1","omega2","pi","psi0","psi1","psi2","qmu","rho","theta","xi","tau","eta","phi1","lambda","lsigma","nu0","nu1","nu2","qP","cx","CU","ag","rx","sigma_obs") #"mu.c","tau.c",


##single chain (debugging)

#    sinits<-make.inits()
##    print(sinits)
#    MRModel <- nimbleModel(code = smoltCode,data = Mdata,constants = Mconsts, inits = sinits,calculate=FALSE)  #,buildDerivs = TRUE) 

#MRModel$simulate()
#MRModel$calculate()
#
#
#  stnodes <- MRModel$getNodeNames(stochOnly = TRUE, includeData = TRUE)
#  allvars<-MRModel$getVarNames(nodes = stnodes)
#  mvars<-allvars[!(grepl("lifted",allvars))]  
#  
#  #set.seed(794648) 
#  MRModel$simulate(MRModel$getDependencies(stnodes))
#  MRModel$calculate()
#  
#  for(i in 1:length(mvars)){
#  print(paste0(mvars[i]," ",MRModel$calculate(mvars[i]) ))
#  }
#  
#
#nimbleOptions(MCMCenableWAIC = TRUE)
#MRConf <- configureMCMC(MRModel, print=TRUE, useConjugacy = FALSE, monitors = parnames)   #useConjugacy = FALSE
#CMR <- compileNimble(MRModel)   #,dbetabin,rbetabin
#mMCMC <- buildMCMC(MRConf) # uncompiled R code
#CMRMCMC <- compileNimble(mMCMC, project = MRModel)  
#
#results <- runMCMC(CMRMCMC, niter = 40000, thin=20, nburnin=20000, WAIC=TRUE)  
#
#chains<-mcmc(results$samples)
#d<-as.matrix(chains)
#dev.new()
#par(mfrow=c(4,2),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
#traceplot(chains[,"CU"])
#traceplot(chains[,"qmu[36]"])
#traceplot(chains[,"nu0"])
#traceplot(chains[,"nu1"])
#traceplot(chains[,"nu2"])
#traceplot(chains[,"rho"])
#traceplot(chains[,"xi"])
#traceplot(chains[,"sigma_obs"])
#

##parallel chains
ptm<-proc.time()
this_cluster <- makeCluster(4,outfile="")
run_SmoltCode <- function(seed,smodel,sdata,sconsts,sinits,smonitor) {
library(nimble) 
source(smodel)

MRModel<- nimbleModel(code = smoltCode, constants = sconsts,  
inits=sinits,data=sdata,calculate=FALSE)
MRModel$simulate()
MRModel$calculate()



  stnodes <- MRModel$getNodeNames(stochOnly = TRUE, includeData = TRUE)
  allvars<-MRModel$getVarNames(nodes = stnodes)
  mvars<-allvars[!(grepl("lifted",allvars))]  
  
  #set.seed(794648) 
  MRModel$simulate(MRModel$getDependencies(stnodes))
  
  for(i in 1:length(mvars)){
     MRModel$calculate(mvars[i]) 
  }
  
##TRY WITHOUT USE CONJUGACY = FALSE
nimbleOptions(MCMCenableWAIC = TRUE)
MRConf <- configureMCMC(MRModel, print=TRUE, useConjugacy = FALSE, monitors = smonitor)   #useConjugacy = FALSE

CMR <- compileNimble(MRModel)   #,dbetabin,rbetabin
mMCMC <- buildMCMC(MRConf) # uncompiled R code
CMRMCMC <- compileNimble(mMCMC, project = MRModel)  

results <- runMCMC(CMRMCMC, niter =  100000, nburnin = 50000, thin=50, WAIC=TRUE)      #1000 per chain
#results <- runMCMC(CMRMCMC, niter =  400000, nburnin = 200000, thin=200, setSeed = seed,WAIC=TRUE)      #1000 per chain
return(results)  

}

chain_output <- parLapply(cl = this_cluster, X = 1:3, 
                          fun = run_SmoltCode,smodel=modelfile,
                          sdata = Mdata,sconsts=Mconsts,sinits=make.inits(),smonitor=parnames)

stopCluster(this_cluster)
proc.time()-ptm

v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)
v3 <- mcmc(chain_output[[3]]$samples)
chains<-mcmc.list(list(v1,v2,v3)) 
d<-as.matrix(chains)



dev.new()
par(mfrow=c(4,2),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
traceplot(chains[,"CU"])
traceplot(chains[,"qmu[36]"])
traceplot(chains[,"nu0"])
traceplot(chains[,"nu1"])
traceplot(chains[,"nu2"])
traceplot(chains[,"rho"])
traceplot(chains[,"xi"])
traceplot(chains[,"sigma_obs"])



dev.new()
par(mfrow=c(1,1),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
plot(density(d[,"CU"]),main="",xlim=c(0,75000),lwd=2)

quantile(d[,"CU"],c(0.025,0.50,0.975))

source("plotfunctions.r")

dev.new()        
par(mfrow=c(1,1),mar=c(3,4,0.1,0.1),oma=c(2,2,0.1,0.1),font=2,font.lab=2,font.axis=2,cex.lab=1,cex.axis=1) 
bx2g_ylim(d,1,N,1,"cx[","]",1,N,0,50,0.25,ylab="Catch")   
points(1:N,catch,pch=17,col="red")


