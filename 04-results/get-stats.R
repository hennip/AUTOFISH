
chains<-as.mcmc.list(run)
chains<-window(chains, start=200000)


#print stats to file
d<-as.matrix(chains)
dim(d)

statsfile<-paste0(path_output,modelname,"_summary.csv")

headtext<-c("mean","sd","cv","5%","50%","95%","90%PI","grdPE", "grdUCI", "Varname")

write.table(t(as.matrix(headtext)),file=statsfile,sep=',',row.names=F, col.names=F)

for(i in 1:dim(d)[2]){ # loop over all monitored variables
  m<-mean(d[,i])
  s<-sd(d[,i])
  cv<-s/m
  q5<-quantile(d[,i],0.05)
  q50<-quantile(d[,i],0.50)
  q95<-quantile(d[,i],0.95)
  PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 in round() if decimals needed
  grdPE<-gelman.diag(chains[,i])$psrf[1]
  grdUCI<-gelman.diag(chains[,i])$psrf[2]
  
  printtxt<-c(m,s,cv,q5,q50,q95,PI90,grdPE, grdUCI, colnames(d)[i])
  write.table(t(as.matrix(printtxt)),statsfile,sep=",",row.names=F, col.names=F,append=T)
}


