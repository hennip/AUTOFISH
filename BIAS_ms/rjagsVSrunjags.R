
# rjags: sample= n.iter/thin
# runjags: sample= sample


test<-"model{x~dnorm(0,1)}"
cat(test,file="test.txt")

jm<-jags.model('test.txt', n.adapt=1000, n.chains=1)

system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "x"
                                  ),
                                  n.iter=600000,
                                  thin=500))

run0<-run.jags(test, monitor=c("x"),n.chains = 1, thin=500,
         sample =600000, adapt = 1000,
         keep.jags.files=F,
         progress.bar=TRUE, jags.refresh=100)


length(chains2[[1]]) # 1200


parnames<-c(
  "x")

run0<- run.jags(test, monitor= parnames,
                     #data=datalist,
                     n.chains = 2, method = 'parallel', thin=500,
                     burnin =10, modules = "mix",
                     sample =600000, adapt = 1000,
                     keep.jags.files=F,
                     progress.bar=TRUE, jags.refresh=100)

chains<-as.mcmc(run0)