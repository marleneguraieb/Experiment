#Merge data from all sessions

library(session,ggplot2,plyr)
library(stargazer)
setwd("~/git_work/Experiment")
restore.session("Aug30.RSession")


Session1<-read.csv("Session1_170816.csv",stringsAsFactors=FALSE)
Session2<-read.csv("Session2_160818.csv",stringsAsFactors=FALSE)
Session3<-read.csv("Session3-230816.csv",stringsAsFactors=FALSE)
Session4<-read.csv("Session4_240816.csv",stringsAsFactors=FALSE,sep=",")
Session5<-read.csv("Session5_260816.csv",stringsAsFactors=FALSE,sep=",")

Session1$Session<-as.factor(1)
Session2$Session<-as.factor(2)
Session3$Session<-as.factor(3)
Session4$Session<-as.factor(4)
Session5$Session<-as.factor(5)
Session1$tmt<-as.character("Strategic")
Session2$tmt<-as.character("Strategic")
Session3$tmt<-as.character("Decision")
Session4$tmt<-as.character("Strategic")
Session5$tmt<-as.character("Strat_HCost")

Session1$InvOther<-NULL
Session1$DiscOther<-NULL
Session1$TypeOther<-NULL
Session1$ThetaOther<-NULL

Session2$InvOther<-NULL
Session2$DiscOther<-NULL
Session2$TypeOther<-NULL
Session2$ThetaOther<-NULL


Strat_lc<-rbind(Session1,Session2,Session4)
Strat_hc<-Session5
Comp<-Session3

rm(Session1,Session2,Session3,Session4,Session5)


#To clean up you take the dataset, call it data, then do the whole thing and then re-call it whatever it was called
DATA<-Strat_lc
#Agent and principal stuff

DATA$Theta[which(DATA$Agent==0)]<- NA
DATA$Applet[which(DATA$Agent==0)]<- NA
DATA$Disclose[which(DATA$Agent==0)]<- NA

DATA$Inv[which(DATA$Agent==1)]<- NA
DATA$Outcome[which(DATA$Agent==1)]<- NA
DATA$Retain[which(DATA$Agent==1)]<- NA

DATA$SPSubj<-paste(DATA$Session,DATA$Period,DATA$Subject,sep="-")
DATA$SPPart<-paste(DATA$Session,DATA$Period,DATA$MyPartner,sep="-")

DATA<-DATA[,setdiff(colnames(DATA),colnames(DATA)[grep("Other",colnames(DATA))])]

DATA$Persubj<-NULL
DATA$Perpart<-NULL

Strat_lc<-DATA
rm(DATA)

DATA<-rbind(Comp,Strat_hc,Strat_lc)

#Let's make all variables 0-1, so that averages work:
DATA$Inv[which(DATA$Inv==1)]<-0
DATA$Inv[which(DATA$Inv==2)]<-1

DATA$Disclose[which(DATA$Disclose==1)]<-0
DATA$Disclose[which(DATA$Disclose==2)]<-1

DATA$Treatment<-NA
DATA$Treatment[which(DATA$tmt=="Decision")]<-0
DATA$Treatment[which(DATA$tmt=="Strategic")]<-1
DATA$Treatment[which(DATA$tmt=="Strat_HCost")]<-2
DATA$tmt<-reorder(DATA$tmt,DATA$Treatment)

#If I invest high and get zero I should kick out (so close to zero):
mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & Period>5)])
mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>15)])
sd(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>5)])
sd(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3)])
#If I invest low and get zero I should retain (so close to one):
mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>15)])
sd(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3)])


length(which(DATA$tmt=="Strategic" & DATA$Inv==2 & DATA$Outcome==3 & DATA$Retain==0))/length(which(DATA$tmt=="Strategic" & DATA$Inv==2 & DATA$Outcome==3))
#If I invest low and get zero I should retain:
length(which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Retain==1))/length(which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3))

#If I get T I should retain:
length(which(DATA$Outcome==2 & DATA$Theta[match(DATA$SPPart,DATA$SPSubj)]==1 & DATA$Retain==1))/
  length(which(DATA$Outcome==2 & DATA$Theta[match(DATA$SPPart,DATA$SPSubj)]==1))

#Dominant strategies:
#1) Good apple, good barrel:
length(which(DATA$Theta==1 & DATA$Applet==1 & DATA$Disclose==2))/length(which(DATA$Theta==1 & DATA$Applet==1))
#2) Good barrel, bad apple
length(which(DATA$Theta==1 & DATA$Applet==0 & DATA$Disclose==2))/length(which(DATA$Theta==1 & DATA$Applet==0))
#4) Bad barrel, bad apple
length(which(DATA$Theta==0 & DATA$Applet==0 & DATA$Disclose==2))/length(which(DATA$Theta==0 & DATA$Applet==0))

#Not dominant strategies
#3) Bad barrel, good apple MisMatch:
#warning, this has very low values bc. there's only two good apples in bad barrel
#only 3 out of 10 got it so far: mismatch
length(which(DATA$Theta==0 & DATA$Applet==1 & DATA$Disclose==1 & DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==2))
length(which(DATA$Theta==0 & DATA$Applet==1 & DATA$Disclose==2 & DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1))


#compare retention rates in empty in strategic and decision sessions:
length(which(Decision$Inv==2 & Decision$Outcome==3 & Decision$Retain==0))/length(which(Decision$Inv==2 & Decision$Outcome==3))
length(which(Decision$Inv==1 & Decision$Outcome==3 & Decision$Retain==0))/length(which(Decision$Inv==2 & Decision$Outcome==3))

length(which(Strategic$Inv==2 & Strategic$Outcome==3 & Strategic$Retain==0))/length(which(Strategic$Inv==2 & Strategic$Outcome==3))
length(which(Strategic$Inv==1 & Strategic$Outcome==3 & Strategic$Retain==0))/length(which(Strategic$Inv==2 & Strategic$Outcome==3))

#lms:
lm1 <- lm(Retain ~ tmt + Outcome + Inv + Outcome:Inv, data=DATA)
lm2 <- lm(Retain ~ Outcome + Inv + Outcome:Inv, data=Decision)

#

#Table of reelection after empty:

Head<-c("Low"," ","High"," ","Low"," ","High"," ","Low"," ","High"," ")
Allper<-c(length(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3)]), 
          mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3)]),
          length(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3)]),
          mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3)]),
          length(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3)]),
          mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3)]),
          length(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3)]),
          mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3)]),
          length(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3)]),
          mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3)]),
          length(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3)]),
          mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3)]))

Per1a10<-c(length(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period<11)]), 
           mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period<11)]),
           length(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period<11)]),
           mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period<11)]),
           length(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period<11)]),
           mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period<11)]),
           length(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period<11)]),
           mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period<11)]),
           length(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period<11)]),
           mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period<11)]),
           length(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period<11)]),
           mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period<11)]))

Per11a20<-c(length(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>10)]), 
            mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>10)]),
            length(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>10)]),
            mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>10)]),
            length(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>10)]),
            mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>10)]),
            length(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>10)]),
            mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>10)]),
            length(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>10)]),
            mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>10)]),
            length(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>10)]),
            mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>10)]))

Theory<-c(0,1,0,0,0,1,0,0,0,1,0,0)

Table<-rbind(Allper,Per1a10,Per11a20,Theory)
colnames(Table)<-Head

cols<-c(2,4,6,8,10,12)
for(i in 1:length(cols)){
  Table[,cols[i]]<-Table[,cols[i]]*100
}

stargazer(Table,digits=1)

#Table of willingness to invest:

Head<-c(" 1","Low cost"," 2","High cost"," 3","Low cost2")

Allper<-c(length(DATA$Inv[which(DATA$tmt=="Strategic" & DATA$Inv>=0)]), 
          mean(DATA$Inv[which(DATA$tmt=="Strategic")],na.rm=T),
          length(DATA$Inv[which(DATA$tmt=="Strat_HCost" & DATA$Inv>=0)]), 
          mean(DATA$Inv[which(DATA$tmt=="Strat_HCost")],na.rm=T),
          length(DATA$Inv[which(DATA$tmt=="Decision" & DATA$Inv>=0)]), 
          mean(DATA$Inv[which(DATA$tmt=="Decision")],na.rm=T))

Per1a10<-c(length(DATA$Inv[which(DATA$tmt=="Strategic" & DATA$Inv>=0 & DATA$Period<11)]), 
          mean(DATA$Inv[which(DATA$tmt=="Strategic" & DATA$Period<11)],na.rm=T),
          length(DATA$Inv[which(DATA$tmt=="Strat_HCost" & DATA$Inv>=0 & DATA$Period<11)]), 
          mean(DATA$Inv[which(DATA$tmt=="Strat_HCost"& DATA$Period<11)],na.rm=T),
          length(DATA$Inv[which(DATA$tmt=="Decision" & DATA$Inv>=0 & DATA$Period<11)]), 
          mean(DATA$Inv[which(DATA$tmt=="Decision"& DATA$Period<11)],na.rm=T))

Per11a20<-c(length(DATA$Inv[which(DATA$tmt=="Strategic" & DATA$Inv>=0 & DATA$Period>10)]), 
           mean(DATA$Inv[which(DATA$tmt=="Strategic" & DATA$Period>10)],na.rm=T),
           length(DATA$Inv[which(DATA$tmt=="Strat_HCost" & DATA$Inv>=0 & DATA$Period>10)]), 
           mean(DATA$Inv[which(DATA$tmt=="Strat_HCost"& DATA$Period>10)],na.rm=T),
           length(DATA$Inv[which(DATA$tmt=="Decision" & DATA$Inv>=0 & DATA$Period>10)]), 
           mean(DATA$Inv[which(DATA$tmt=="Decision"& DATA$Period>10)],na.rm=T))

Theory<-c(0,1,0,0,0,0)

Table<-rbind(Allper,Per1a10,Per11a20,Theory)
colnames(Table)<-Head

cols<-c(2,4,6)
for(i in 1:length(cols)){
  Table[,cols[i]]<-Table[,cols[i]]*100
}

stargazer(Table[,c(2,4,6)],digits=1)



#Revelation strategies
#Good apple, good barrel; bad apple good barrel, good apple, bad barrel, bad apple bad barrel.
#TMT1 low cost, invH: rH, rMM,rH,rL-> rH,rL,x,rL
#TMT2 high cost, rH,rL,rM,rL       ->rH,rL,x,rL



Head<-c("GG","GGhc","BG","BGhc","GB","GBhc","BB","BBhc")

Allper<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==1)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==1)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==0)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==1)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==1)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==0)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==0)]))

Per1a10<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==1 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==1 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==0 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==1 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==1 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==0 & DATA$Period<11)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==0 & DATA$Period<11)]))

Per11a20<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==1 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==1 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==0 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==1 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==1 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==0 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==0 & DATA$Period>10)]))

Theory<-c(1,1,0,0,1,0,0,0)

Table<-rbind(Allper,Per1a10,Per11a20,Theory)
colnames(Table)<-Head

cols<-c(1:8)
for(i in 1:length(cols)){
  Table[,cols[i]]<-Table[,cols[i]]*100
}

stargazer(Table,digits=1)

N<-c(length(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==1)]),
     length(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==1)]),
     length(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0)]),
     length(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==1 & DATA$Applet==0)]),
     length(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==1)]),
     length(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==1)]),
     length(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==0 & DATA$Applet==0)]),
     length(DATA$Disclose[which(DATA$tmt=="Strat_HCost" & DATA$Theta==0 & DATA$Applet==0)]))

#Now let's see for type two:
#these should be different: rMM
#this one high investment should be low and low investment should be high:
Head<-c("High investment","Low investment")

Allper<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                           DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                           DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0)]))

Per1a10<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                                              DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1 & DATA$Period<11)]),
                   mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                                              DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0 & DATA$Period<11)]))

Per11a20<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                                      DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1 & DATA$Period>10)]),
           mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                                      DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0 & DATA$Period>10)]))

dif1<-c(0,t.test(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                             DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1)],
       DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                             DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0)])[[3]])

dif2<-c(0,t.test(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                                       DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1 & DATA$Period<11)],
                 DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                                       DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0 & DATA$Period<11)])[[3]])

dif3<-c(0,t.test(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                                       DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1 & DATA$Period>10)],
                 DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                                       DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0 & DATA$Period>10)])[[3]])

Theory<-c(0,1)

Table<-rbind(Allper,dif1,Per1a10,dif2,Per11a20,dif3,Theory)
colnames(Table)<-Head

cols<-c(1:2)
for(i in 1:length(cols)){
  Table[,cols[i]]<-Table[,cols[i]]*100
}

N<-c(length(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                                     DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1)]),
          length(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                                     DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0)]))

Table<-rbind(Table,N)
stargazer(Table,digits=1)


#Time series:
Strat_lc_r0gli<-rep(0,5)
Strat_hc_r0gli<-rep(0,5)
Comp_lc_r0gli<-rep(0,5)
Strat_lc_r0ghi<-rep(0,5)
Strat_hc_r0ghi<-rep(0,5)
Comp_lc_r0ghi<-rep(0,5)


pers<-c(0,4,8,12,16,20)
for(i in 1:(length(pers)-1)){
  Strat_lc_r0gli[i]<-
    mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>pers[i] &  DATA$Period<=pers[i+1])])
  Strat_hc_r0gli[i]<-
    mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>pers[i] &  DATA$Period<=pers[i+1])])
  Comp_lc_r0gli[i]<-
    mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>pers[i] &  DATA$Period<=pers[i+1])])
  Strat_lc_r0ghi[i]<-
    mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>pers[i] &  DATA$Period<=pers[i+1])])
  Strat_hc_r0ghi[i]<-
    mean(DATA$Retain[which(DATA$tmt=="Strat_HCost" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>pers[i] &  DATA$Period<=pers[i+1])])
  Comp_lc_r0ghi[i]<-
    mean(DATA$Retain[which(DATA$tmt=="Decision" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>pers[i] &  DATA$Period<=pers[i+1])])
  
}
 
#PLOTS:

means <- ddply(DATA[which(DATA$Inv>=0 & DATA$Outcome==3 & DATA$Period>5),], c("tmt", "Inv"), summarise, Retain=mean(Retain,na.rm=T))
means$Inv<-as.factor(means$Inv)
theory<-c(1,1,1,0,1,0)
means$theory<-theory
means$distance<-means$theory-means$Retain
colnames(means)<-c("Treatment","Information","Retention","Theory","Distance")
levels(means$Information)[1]<-"Low"
levels(means$Information)[2]<-"High"
levels(means$Treatment)[1]<-"TMT0: Decision"
levels(means$Treatment)[2]<-"TMT1: S_LC"
levels(means$Treatment)[3]<-"TMT2: S_HC"
ggplot(means,aes(Treatment,Retention,fill=Information))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")

pdf("ret.pdf")
ggplot(means,aes(Treatment,Retention,fill=Information))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")+theme(axis.title.x = element_blank())
dev.off()

meansinv <- ddply(DATA[which(DATA$Inv>=0 & DATA$Outcome==3),], "tmt", summarise, Information=mean(Inv,na.rm=T))
theory<-c(0,1,0)
meansinv$theory<-theory
meansinv$distance<-meansinv$theory-meansinv$Information
colnames(meansinv)<-c("Treatment","Information","Theory","Distance")
levels(meansinv$Treatment)[1]<-"TMT0: Decision"
levels(meansinv$Treatment)[2]<-"TMT1: S_LC"
levels(meansinv$Treatment)[3]<-"TMT2: S_HC"
ggplot(meansinv,aes(Treatment,Information))+
  geom_bar(stat="identity")+scale_fill_brewer(palette="BuPu")

pdf("inv.pdf")
ggplot(meansinv,aes(Treatment,Information,fill=T))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")+theme(axis.title.x = element_blank())+ guides(fill=FALSE)+scale_y_continuous(limits = c(0,1))
dev.off()

#Agents:
#dominant strategies
means <- ddply(DATA[which(DATA$Disclose>=0  & DATA$tmt!="Decision" & DATA$Theta==DATA$Applet),], c("tmt", "Theta","Applet"), summarise, Disclose=mean(Disclose,na.rm=T))
means$Theta<-as.factor(means$Theta)
means$Applet<-as.factor(means$Applet)
theory<-c(0,1,0,1)
means$theory<-theory
means$distance<-means$theory-means$Disclose
colnames(means)<-c("Treatment","State","Type","Disclose","Theory","Distance")
levels(means$Type)[1]<-"Bad/bad types"
levels(means$Type)[2]<-"Good/good types"
levels(means$Treatment)[1]<-"TMT0: Decision"
levels(means$Treatment)[2]<-"TMT1: S_LC"
levels(means$Treatment)[3]<-"TMT2: S_HC"
ggplot(means,aes(Treatment,Disclose,fill=Type))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")

pdf("discl.pdf")
ggplot(means,aes(Treatment,Disclose,fill=Type))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")
dev.off()

#Bad types good barrel
Disclose<-c(mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 &
                                     DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0)]),
          mean(DATA$Disclose[which(DATA$tmt=="Strategic" & DATA$Theta==1 & DATA$Applet==0 & 
                                     DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1)]))
Information<-c("Low information","High information")

Theory<-c(1,0)
  
strat<-as.data.frame(cbind(Inv,Disclose,Theory))
strat$Disclose<-as.numeric(levels(strat$Disclose))[strat$Disclose]

ggplot(strat,aes(Information,Disclose,fill=T))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")+
  theme(axis.title.x = element_blank())+ guides(fill=FALSE)+scale_y_continuous(limits = c(0,1))


pdf("strat.pdf")
ggplot(strat,aes(Information,Disclose,fill=T))+
  geom_bar(position="dodge",colour="black",stat="identity")+scale_fill_brewer(palette="BuPu")+
  theme(axis.title.x = element_blank())+ guides(fill=FALSE)+scale_y_continuous(limits = c(0,1))
dev.off()
