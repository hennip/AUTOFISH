
# rjags: sample= n.iter/thin
# runjags: sample= sample


y<-seq(-5,5, by=0.2)
x<-exp(y)/(1+exp(y))

test<-"model{

x1~dbeta(2,2)
x2~dbeta(1,1)

y~dnorm(0,0.5)
x3<-exp(y)/(1+exp(y))

z~dlnorm(0.8,0.1)

}"
cat(test,file="test.txt")

# jm<-jags.model('test.txt', n.adapt=1000, n.chains=1)
# 
# system.time(chains2<-coda.samples(jm,
#                                   variable.names=c(
#                                     "x"
#                                   ),
#                                   n.iter=600000,
#                                   thin=500))



# run0<- run.jags(test, monitor= parnames,
#                      #data=datalist,
#                      n.chains = 2, method = 'parallel', thin=500,
#                      burnin =10, modules = "mix",
#                      sample =600000, adapt = 1000,
#                      keep.jags.files=F,
#                      progress.bar=TRUE, jags.refresh=100)

parnames<-c(
  "y", "x1", "x2", "x3", "z")

run0<- run.jags(test, monitor= parnames,
                #data=datalist,
                n.chains = 2, method = 'parallel', thin=1,
                burnin =10, modules = "mix",
                sample =10000, adapt = 1000,
                keep.jags.files=F,
                progress.bar=TRUE, jags.refresh=100)

summary(run0)
plot(run0)

chains<-as.mcmc(run0)