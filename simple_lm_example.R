library(runjags)
library(coda)

x=c(1,1,2,3,4,4,5,5,6,7)
y=c(0.1,0.3,0.5,0.6,0.6,0.65,0.7,0.75,0.8,0.9)

plot(x,y)

m1<-lm(y~x)
abline(m1)
m1



m2<-"model{
a~dunif(-10,10)
b~dunif(-10,10)
sd~dunif(0.001,10)

for(i in 1:N){
y[i]~dnorm(mu[i], 1/(sd*sd))
mu[i]<-a+b*x[i]
}

# Unupdated priors for comparison
aX~dunif(-10,10)
bX~dunif(-10,10)
sdX~dunif(0.001,10)


}"

y=c(0.1,0.3,0.5,0.6,0.6,0.65,0.7,NA,0.8,0.9)


run_no_NA<- run.jags(m2, monitor= c("a", "b", "sd","y",
                               "aX", "bX", "sdX"),
                data=list(x=x,y=y, N=length(x)),
                n.chains = 2, method = 'parallel', thin=1,
                burnin =10, modules = "mix",
                sample =10000, adapt = 1000,
                keep.jags.files=F,
                progress.bar=TRUE, jags.refresh=100)
run<-run_no_NA
run<-run0

summary(run)
plot(run, var="a")

chains<-as.mcmc(run)

par(mfrow=c(2,2))
plot(density(chains[,"a"]), main="a", xlab="")
lines(density(chains[,"aX"]))
plot(density(chains[,"b"]), main="b", xlab="")
lines(density(chains[,"bX"]))
plot(density(chains[,"sd"]), main="sd", xlab="")
lines(density(chains[,"sdX"]))
plot(density(chains[,"y[8]"]), main="y[8]", xlab="")

a<-chains[,"a"][1:100]
b<-chains[,"b"][1:100]
y8<-chains[,"y[8]"][1:100]
n<-length(chains[,"a"][1:100])
par(mfrow=c(1,1))
plot(x,y)
for(i in 1:n){
abline(a[i],b[i], col=rgb(0, 0, 1, 0.1))
  points(x[8], y8[i],col=rgb(1, 0, 0, 0.3))
}

