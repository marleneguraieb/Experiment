

#If I invest high and get zero I should kick out (so close to zero):
mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>5)])
mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>15)])
sd(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Period>5)])
sd(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3)])
#If I invest low and get zero I should retain (so close to one):
mean(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Period>15)])
sd(DATA$Retain[which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3)])


length(which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3 & DATA$Retain==0))/length(which(DATA$tmt=="Strategic" & DATA$Inv==1 & DATA$Outcome==3))
#If I invest low and get zero I should retain:
length(which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3 & DATA$Retain==1))/length(which(DATA$tmt=="Strategic" & DATA$Inv==0 & DATA$Outcome==3))

#If I get T I should retain:
length(which(DATA$Outcome==2 & DATA$Theta[match(DATA$SPPart,DATA$SPSubj)]==1 & DATA$Retain==1))/
  length(which(DATA$Outcome==2 & DATA$Theta[match(DATA$SPPart,DATA$SPSubj)]==1))

#Dominant strategies:
#1) Good apple, good barrel:
length(which(DATA$Theta==1 & DATA$Applet==1 & DATA$Disclose==1))/length(which(DATA$Theta==1 & DATA$Applet==1))
#2) Good barrel, bad apple
length(which(DATA$Theta==1 & DATA$Applet==0 & DATA$Disclose==1))/length(which(DATA$Theta==1 & DATA$Applet==0))
#4) Bad barrel, bad apple
length(which(DATA$Theta==0 & DATA$Applet==0 & DATA$Disclose==1))/length(which(DATA$Theta==0 & DATA$Applet==0))

#Not dominant strategies
#3) Bad barrel, good apple MisMatch:
#warning, this has very low values bc. there's only two good apples in bad barrel
#only 3 out of 15 got it so far: mismatch
length(which(DATA$Theta==0 & DATA$Applet==1 & DATA$Disclose==0 & DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==1))
length(which(DATA$Theta==0 & DATA$Applet==1 & DATA$Disclose==1 & DATA$Inv[match(DATA$SPPart,DATA$SPSubj)]==0))


#compare retention rates in empty in strategic and decision sessions:
length(which(DATA$Treatment==0 & DATA$Inv==1 & DATA$Outcome==3 & DATA$Retain==0))/length(which(DATA$Treatment==0 & DATA$Inv==1 & DATA$Outcome==3))
length(which(DATA$Treatment==0 & DATA$Inv==0 & DATA$Outcome==3 & DATA$Retain==0))/length(which(DATA$Treatment==0 & DATA$Inv==0 & DATA$Outcome==3))

length(which(Strategic$Inv==2 & Strategic$Outcome==3 & Strategic$Retain==0))/length(which(Strategic$Inv==2 & Strategic$Outcome==3))
length(which(Strategic$Inv==1 & Strategic$Outcome==3 & Strategic$Retain==0))/length(which(Strategic$Inv==2 & Strategic$Outcome==3))

#lms:
lm1 <- lm(Retain ~ tmt + Outcome + Inv + Outcome:Inv:tmt, data=DATA)
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
