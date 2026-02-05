# ECOKNOWS Herring case study:
# Model for the acoustic survey (BIAS)

# This file prepares the age data of herring
# into a form that can be inputted to the BIAS model (current version BIAS_09_12.r)

# Output files:    none, need to run this file every time BIAS model is run

# Data preparations made in Excel:
# * Remove data rows for which Age==NA or TotalLength==NA

# All that is needed are the haul numbers (need to be combined with 
# the statistical rectangles, use corresponding Acol-file), Age and TotalLength.

#setwd("C:/Projects/HerringCase/BIAS")
 
dat<-read.table("BIAS_ms/AgeData2009.txt", header=T)
dim(dat)
summary(dat)
summary(as.factor(dat$Haul))

################################################################################
# Check from the length data which haul corresponds to 
# each statistical rectangle
datHauls<-read.table("BIAS_ms/HaulsAndRectangles2009.txt", header=T)
summary(datHauls)

# Because of the sampling scheme, we need to use all age data, 
# not just those from the night hauls!
#datHauls<-subset(datHauls, DayNight=="N")
NH<-length(summary(as.factor(datHauls$Haul)))
datHauls$Haul2<-factor(datHauls$Haul, labels=c(1:NH))

rectangles<-c(#"49G8","49G9",
"50G7","50G8","50G9","50H0","50H1",
"51G7","51G8","51G9","51H0","51H1",
"52G7","52G8","52G9","52H0","52H1",
"53G7","53G8","53G9","53H0","53H1",
"54G7","54G8","54G9","54H0","55G8",
"55G9","55H0","55H1")
NR<-length(rectangles) # 28
Rectangles<-cbind(rectangles, 1:28)

rect<-vector()
for(i in 1:length(datHauls$ICES)){
  for(j in 1:NR){
    if(datHauls$ICES[i]==Rectangles[j,1]){
      rect[i]<-as.numeric(Rectangles[j,2])          
    }
  }
}
datHauls$rect<-rect

# Make a (short) list of hauls and rectangles
HR<-array(NA, dim=c(NH,2))
for(j in 1:NH){
  temp<-0
  while(temp==0){
    for(i in 1:length(datHauls$ICES)){
      if(datHauls$Haul2[i]==j){
        HR[j,1]<-datHauls$Haul[i]
        HR[j,2]<-datHauls$rect[i]    
        temp<-1
      }
    }
  }
}
HR
colnames(HR)<-c("haul","rectangle")
HR<-as.data.frame(HR)
HR$haul

################################################################################
rect<-rep(NA,length(dat$Haul))
for(i in 1:length(dat$Haul)){
  for(j in 1:length(HR$haul)){
    if(dat$Haul[i]==HR$haul[j]){
      rect[i]<-HR$rectangle[j]
    }
  }
}
dat$rect<-rect
summary(dat)
dat<-subset(dat, is.na(rect)==F)

# 8 length classess, 9 breaking points
LengthIntervals<-c(5,7.5,10,12.5,15,17.5,20,22.5,25)                                                # 8 classes
LI<-LengthIntervals


# AgeL stores number of herring in each age class (0-10+) for each
# length class and per rectangle
AgeL<-array(NA, dim=c(9,8,NR))
for(r in 1:NR){
  for(l in 1:8){
    n<-0
    m<-rep(0,9)

    for(i in 1:length(dat$Haul)){
      if(dat$rect[i]==r){

        if(l==1){
        if(dat$TotalLength[i]/10 < LI[2]){
          for(a in 1:8){ # ages 0 - 7
            if(dat$Age[i]==(a-1)){m[a]<-m[a]+1 ; n<-n+1}
          }
          if(dat$Age[i]>7){m[9]<-m[9]+1 ; n<-n+1} # ages 8+
        }
        }

        if(l>1 && l<8){
        if(dat$TotalLength[i]/10 >= LI[l] && dat$TotalLength[i]/10 < LI[l+1]){
          for(a in 1:8){ # ages 0 - 9
            if(dat$Age[i]==(a-1)){m[a]<-m[a]+1;n<-n+1}
          }
          if(dat$Age[i]>7){m[9]<-m[9]+1 ; n<-n+1} # ages 8+
        }
        }

        if(l==8){
        if(dat$TotalLength[i]/10 >= LI[8]){
          for(a in 1:8){ # ages 0 - 9
            if(dat$Age[i]==(a-1)){m[a]<-m[a]+1 ; n<-n+1}
          }
          if(dat$Age[i]>7){m[9]<-m[9]+1 ; n<-n+1} # ages 8+
        }
        }

      }
    }
    for(a in 1:9){
      if(n>0){AgeL[a,l,r]<-m[a]}
      if(n==0){AgeL[a,l,r]<-NA}
    }
  }
}
AgeL

sum(AgeL, na.rm=T)==dim(dat)[1]

Age<-AgeL

#Age<-array(NA, dim=c(11,8,NR))
AgeTot<-array(100, dim=c(8,NR))

for(r in 1:NR){
  for(l in 1:8){
    AgeTot[l,r]<-sum(Age[1:9,l,r])
  }
}

for(r in 1:NR){
  for(l in 1:8){
    if(is.na(AgeTot[l,r])==T){AgeTot[l,r]<-100}
  }
}
AgeTot

Age09<-Age
AgeTot09<-AgeTot

rm(Age, AgeTot, dat)


