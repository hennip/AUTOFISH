# Datasets for BIAS model
NumYears<-6 # 2007:2012

# Areas of rectangles in NM2
#############################
# (check that areas are treated the same way in AcousticData.r-file)
#areas<-read.table("prg/input/d_areas.txt", header=T)
#A<-areas$area #NM2
#Atot<-17169.8 #NM2                                          
# areas of rectangles in nm2
areas<-read.table("BIAS_ms/d_areas.txt", header=T)[3:30,] # leave out first 2!
A<-areas$area #nm2
Atot<-sum(areas$area)# 16409.6 #nm2



# SA-data
###########
echo07<-read.table("BIAS_ms/d_echo2007.txt", header=T)
echo07$Y<-rep(1,dim(echo07)[1])

echo08<-read.table("BIAS_ms/d_echo2008.txt", header=T)
echo08$Y<-rep(2,dim(echo08)[1])

echo09<-read.table("BIAS_ms/d_echo2009.txt", header=T)
echo09$Y<-rep(3,dim(echo09)[1])

echo10<-read.table("BIAS_ms/d_echo2010.txt", header=T)
echo10$Y<-rep(4,dim(echo10)[1])

echo11<-read.table("BIAS_ms/d_echo2011.txt", header=T)
echo11$Y<-rep(5,dim(echo11)[1])

echo12<-read.table("BIAS_ms/d_echo2012.txt", header=T)
echo12$Y<-rep(6,dim(echo12)[1])


echo<-rbind(echo07,echo08,echo09,echo10,echo11,echo12)
dim(echo)

# Number of nautical miles surveyed per rectangle and per year
##############################################################
Nlog<-array(NA, dim=c(28,NumYears))
Nlog[,1]<-read.table("BIAS_ms/d_LogPerRec2007.txt", header=T)$Nlog
Nlog[,2]<-read.table("BIAS_ms/d_LogPerRec2008.txt", header=T)$Nlog
Nlog[,3]<-read.table("BIAS_ms/d_LogPerRec2009.txt", header=T)$Nlog
Nlog[,4]<-read.table("BIAS_ms/d_LogPerRec2010.txt", header=T)$Nlog
Nlog[,5]<-read.table("BIAS_ms/d_LogPerRec2011.txt", header=T)$Nlog
Nlog[,6]<-read.table("BIAS_ms/d_LogPerRec2012.txt", header=T)$Nlog

# Add two imaginary echo areas to rectangles that do not contain any
for(y in 1:NumYears){
  for(r in 1:28){
    if(Nlog[r,y]==0){Nlog[r,y]<-2}
  }
}

# Length distribution data
###########################
# L: Observed number of fish from each of six rectangles from which night hauls available
# Ltot: Total number of fish caught from each rectangle
# Hprop: Proportion of herring
source("BIAS_ms/trawlData2007.r")
source("BIAS_ms/trawlData2008.r")
source("BIAS_ms/trawlData2009.r")
source("BIAS_ms/trawlData2010.r")
source("BIAS_ms/trawlData2011.r")
source("BIAS_ms/trawlData2012.r")

L<-array(NA,dim=c(8,28,2,NumYears))
L[,,,1]<-L07
L[,,,2]<-L08
L[,,,3]<-L09
L[,,,4]<-L10
L[,,,5]<-L11
L[,,,6]<-L12

Ltot<-array(NA, dim=c(28,2,NumYears))
Ltot[,,1]<-Ltot07
Ltot[,,2]<-Ltot08
Ltot[,,3]<-Ltot09
Ltot[,,4]<-Ltot10
Ltot[,,5]<-Ltot11
Ltot[,,6]<-Ltot12

# Proportions of herring
Ntot<-array(NA, dim=c(28,NumYears))
Ntot[,1]<-Hprop07[,1]
Ntot[,2]<-Hprop08[,1]
Ntot[,3]<-Hprop09[,1]
Ntot[,4]<-Hprop10[,1]
Ntot[,5]<-Hprop11[,1]
Ntot[,6]<-Hprop12[,1]

Nherring<-array(NA, dim=c(28,NumYears))
Nherring[,1]<-Hprop07[,2]
Nherring[,2]<-Hprop08[,2]
Nherring[,3]<-Hprop09[,2]
Nherring[,4]<-Hprop10[,2]
Nherring[,5]<-Hprop11[,2]
Nherring[,6]<-Hprop12[,2]

HerringProp<-array(NA, dim=c(28,NumYears))
for(y in 1:NumYears){
  HerringProp[,y]<-Nherring[,y]/Ntot[,y]
}

# Midpoints of length classes
#############################
meanL<-c(6.25,8.75,11.25,13.75,16.25,18.75,21.25,23.75)

star<-rep(1,8)

# Age data
##############################

star2<-rep(1,9)
                                   
# Age: Observed number of herring in each age class (1:11) per length class (1:8). 
source("BIAS_ms/AgeData2007_9groups.r")
source("BIAS_ms/AgeData2008_9groups.r")
source("BIAS_ms/AgeData2009_9groups.r")
source("BIAS_ms/AgeData2010_9groups.r")
source("BIAS_ms/AgeData2011_9groups.r")
source("BIAS_ms/AgeData2012_9groups.r")



Age<-array(NA, dim=c(9,8,28,NumYears))
Age[,,,1]<-Age07
Age[,,,2]<-Age08
Age[,,,3]<-Age09
Age[,,,4]<-Age10
Age[,,,5]<-Age11
Age[,,,6]<-Age12

AgeTot<-array(100, dim=c(8,28,NumYears))
AgeTot[,,1]<-AgeTot07
AgeTot[,,2]<-AgeTot08
AgeTot[,,3]<-AgeTot09
AgeTot[,,4]<-AgeTot10
AgeTot[,,5]<-AgeTot11
AgeTot[,,6]<-AgeTot12

