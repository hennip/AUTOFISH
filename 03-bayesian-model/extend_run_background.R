source("00-basics/packages-and-paths.R")

modelname<-deparse(substitute(GRAHS4_12))

load(paste0(path_output,modelname,".RData")) # contains run

sink(paste0("sink_",modelname,"_",".txt"))
modelname

t31<-Sys.time();print(t31)
run3 <- extend.jags(run, combine=T, #add.monitor = c("deviance"),
                    sample=500000, thin=100, keep.jags.files=F)
t32<-Sys.time();print(t32)
print("run3 done"); print(difftime(t31,t32))
print("--------------------------------------------------")
run<-run3
save(run, file=paste0(path_output,modelname,".RData"))

sink()
