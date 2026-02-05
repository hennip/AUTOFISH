
test<-model{x~dnorm(0,1)}
cat(BIAS,file="test.txt")

jm<-jags.model('test.txt', n.adapt=1000, n.chains=1)

system.time(chains2<-coda.samples(jm,
                                  variable.names=c(
                                    "x"
                                  ),
                                  n.iter=600000,
                                  thin=500))

save()