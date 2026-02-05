# ECOKNOWS Herring case study:
# Model for the acoustic survey (BIAS)

# This file prepares night trawl haul data into a form that can be inputted
# to the BIAS model. 

# Output files: none, need to run this file every time BIAS model is run

# Data preparations for Acol-file made in Excel:

# * remove day hauls
# * remove spaces from species names. 
# * Indicate Species==1 for herring (clupea harengus), ==2 for other species.
# * (Remove unnecessary columns)



#setwd("C:/Projects/HerringCase/BIAS/")

dat<-read.table("BIAS_ms/NightHauls2012.txt", header=T)

summary(dat)
summary(as.factor(dat$ICES))

rectangles<-c(#"49G8","49G9",
"50G7","50G8","50G9","50H0","50H1",
"51G7","51G8","51G9","51H0","51H1",
"52G7","52G8","52G9","52H0","52H1",
"53G7","53G8","53G9","53H0","53H1",
"54G7","54G8","54G9","54H0","55G8",
"55G9","55H0","55H1")
NR<-length(rectangles) # 28


rect<-c()
for(i in 1:(dim(dat)[1])){
  for(j in 1:NR){
    if(dat$ICES[i]==rectangles[j]){rect[i]<-j}
  }
}    
cbind(dat, rect)
dat$rect<-rect

LengthIntervals<-c(5,7.5,10,12.5,15,17.5,20,22.5,25)                                                # 8 classes
#length(LengthIntervals)
# set the length values (now midpoints) for the intervals
MidPoints<-c() 
for(i in 1:(length(LengthIntervals)-1)){
MidPoints[i]<-(LengthIntervals[i+1]+LengthIntervals[i])/2
}
MidPoints # This needs to be same as meanL in BIAS-data!
length(MidPoints)

dat2<-subset(dat,Species==1)
sum(dat2$No)

LC<-array(0, dim=c(length(MidPoints),NR,2))
for(s in 1:2){ # species: 1=herring, 2=other species
for(r in 1:NR){ # rectangles
    for(j in 1:length(MidPoints)){ # length classes
      for(i in 1:length(dat$No)){ # rows
        if(dat$Species[i]==s & dat$rect[i]==r){
         
         if(abs(dat$Lengthcl[i]-MidPoints[j])<1.25){
          LC[j,r,s]<-dat$No[i]+LC[j,r,s]
         }
         if((dat$Lengthcl[i]-MidPoints[j])==1.25){
          LC[j,r,s]<-dat$No[i]+LC[j,r,s]
         }
         
         if(j==1){ # 1st length class
          if((dat$Lengthcl[i]-MidPoints[j])<=-1.25){
            LC[j,r,s]<-dat$No[i]+LC[j,r,s]
          }
         }
         if(j==length(MidPoints)){ # last length class
          if((dat$Lengthcl[i]-MidPoints[j])>1.25){
            LC[j,r,s]<-dat$No[i]+LC[j,r,s]
          }
         }
         
        }
      }
    }
}
}

LC
LC<-round(LC)


LCtot<-array(1000, dim=c(NR,2))
for(s in 1:2){
  for(r in 1:NR){
    LCtot[r,s]<-sum(LC[,r,s])
  }
}
LCtot
sum(LCtot[,1])

# T?yt? 1000:lla ruudut joista ei ole pituusdataa
for( i in 1:NR){
  for(j in 1:2){
    if(LCtot[i,j]==0){LCtot[i,j]<-1000}
  } 
}
LCtot

Ltot<-LCtot
Ltot
Ltot12<-Ltot

# T?yt? NA:lla ruudut joista ei ole pituusdataa
for(r in 1:NR){
  for(s in 1:2){
    if(sum(LC[1:8,r,s])==0){LC[1:8,r,s]<-rep(NA,8)}
}}

L<-LC
L
L12<-L

Ntot<-rep(NA, NR)
Nherring<-rep(NA, NR)

for(r in 1:NR){
  N<-0
  Nh<-0
  for(i in 1:(dim(dat)[1])){
    if(dat$rect[i]==r){
      N<-N+dat$No[i]
      if(dat$Species[i]==1){Nh<-Nh+dat$No[i]}
    }
  }
  Ntot[r]<-N
  Nherring[r]<-Nh
  if(N==0){Ntot[r]<-1000}
  if(Nh==0){Nherring[r]<-NA}
}
Hprop<-as.data.frame(round(cbind(Ntot, Nherring),0))
Hprop12<-Hprop

rm(Ltot,L,Hprop)


