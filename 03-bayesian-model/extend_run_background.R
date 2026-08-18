source("00-basics/packages-and-paths.R")

run_name<-"GRAHS4_etaE4etaR4_2020-2025"

load(paste0(path_output,run_name,".RData")) # contains run

sink(paste0("sink_extend_",run_name,"_",".txt"))
run_name

t101<-Sys.time();print(t101)
run10 <- extend.jags(run, combine=T, 
                    sample=10000, thin=100, keep.jags.files=F)
t102<-Sys.time();print(102)
print("run10 done"); print(difftime(t101,t102))
print("--------------------------------------------------")
run<-run10
save(run, file=paste0(path_output,run_name,".RData"))

sink()
