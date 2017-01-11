#Merge data from all sessions
#modification 2

library(session,ggplot2,plyr)
library(stargazer)
setwd("~/git_work/Experiment")
restore.session("Aug30.RSession")


Session1<-read.csv("session1_170816.csv",stringsAsFactors=FALSE)
Session2<-read.csv("session2_160818.csv",stringsAsFactors=FALSE)
Session3<-read.csv("session3-230816.csv",stringsAsFactors=FALSE)
Session4<-read.csv("session4_240816.csv",stringsAsFactors=FALSE,sep=",")
Session5<-read.csv("session5_260816.csv",stringsAsFactors=FALSE,sep=",")
Session6<-read.csv("session6_171116.csv",stringsAsFactors=FALSE,sep=",")
Session7<-read.csv("session7_171116.csv",stringsAsFactors=FALSE,sep=",")
Session8<-read.csv("session8_221116.csv",stringsAsFactors=FALSE,sep=",")

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
Session6$Treatment<-as.character("Strategic")
Session7$Treatment<-as.character("Decision")
Session8$Treatment<-as.character("Decision")

Session8<-Session8[,colnames(DATA)]

Strat_lc<-rbind(Session1,Session2,Session4,Session6)
Strat_hc<-Session5
Comp<-Session3

rm(Session1,Session2,Session3,Session4,Session5)


#To clean up you take the dataset, call it data, then do the whole thing and then re-call it whatever it was called
DATA<-Comp
#DATA<-Strat_lc
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

#Strat_lc<-DATA
Comp<-DATA
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

write.csv(DATA, file = "all_data.csv")