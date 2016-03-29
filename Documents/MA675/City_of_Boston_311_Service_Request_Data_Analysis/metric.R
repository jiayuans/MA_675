library(lubridate)
library(plyr)
library("data.table")
qos<-read.csv("dataCleaning311.csv",header = TRUE,stringsAsFactors = F)
qos <- data.table(qos)

#how many open_DT is NA
no<-subset(qos,is.na(qos$OPEN_DT)==T)#only one

qos2014<-qos[grep("/2014",qos$OPEN_DT),]

table(qos2014$neighborhood)
#get rid of c-o na and efficacy2 na
qos2014<-subset(qos2014,is.na(qos2014$TmO)==F)
qos2014<-subset(qos2014,is.na(qos2014$CmO)==F)


qos2014$TmO<-as.numeric(qos2014$TmO)
qos2014$CmO<-as.numeric(qos2014$CmO)

#calculate x%
#x is the mean(CmO)/mean(TmO)

#subset every neighbourhood
AB<-subset(qos2014,qos2014$neighborhood=="Allston / Brighton")
backbay<-subset(qos2014,qos2014$neighborhood=="Back Bay")
beaconhill<-subset(qos2014,qos2014$neighborhood=="Beacon Hill")
charlestown<-subset(qos2014,qos2014$neighborhood=="Charlestown")
dorchester<-subset(qos2014,qos2014$neighborhood=="Dorchester")
downtown<-subset(qos2014,qos2014$neighborhood=="Downtown / Financial District")
eastboston<-subset(qos2014,qos2014$neighborhood=="East Boston")
fkal<-subset(qos2014,qos2014$neighborhood=="Fenway / Kenmore / Audubon Circle / Longwood") 
hydepark<-subset(qos2014,qos2014$neighborhood=="Hyde Park")
jamaicaplain<-subset(qos2014,qos2014$neighborhood=="Jamaica Plain")
mattapan<-subset(qos2014,qos2014$neighborhood=="Mattapan")
missionhill<-subset(qos2014,qos2014$neighborhood=="Mission Hill")
roslindale<-subset(qos2014,qos2014$neighborhood=="Roslindale")
roxbury<-subset(qos2014,qos2014$neighborhood=="Roxbury")
southboston<-subset(qos2014,qos2014$neighborhood=="South Boston / South Boston Waterfront")
southend<-subset(qos2014,qos2014$neighborhood=="South End")
westroxbury<-subset(qos2014,qos2014$neighborhood=="West Roxbury")


##function
#time_consume is the mean of close time-open time in this neighborhood
#consistency, is the standard deviation of the C-O in this neighborhood
#efficacy  is the percentage of job completed over 34.4%of Target tiem-Open time in this neighborhood
#request_N is the total request number in this neighborhood
#ontime_rate is the ontime_request / total_request in this neghborhood(seems the same as efficacy)
df<-function(dataframe){
  average<-mean(dataframe$CmO)/60
  t<-mean(dataframe$TmO)/60
  consistency<-sd(dataframe$CmO)/60
  efficacy<-average/(0.344*t)
  ontime<-subset(dataframe,dataframe$OnTime_Status=="ONTIME")
  ontime_rate<-length(ontime$X)/length(dataframe$X)
  request_N<-length(dataframe$X)
  d<-data.frame(name=dataframe$neighborhood[1],time_consume=average,consistency=consistency,efficacy=efficacy,ontime_rate=ontime_rate,request_N=request_N)
  return(d)
  
}
ab<-df(AB)
bb<-df(backbay)
bh<-df(beaconhill)
ct<-df(charlestown)
dc<-df(dorchester)
dt<-df(downtown)
eb<-df(eastboston)
fk<-df(fkal)
hp<-df(hydepark)
jc<-df(jamaicaplain)
mp<-df(mattapan)
mh<-df(missionhill)
rl<-df(roslindale)
rb<-df(roxbury)
sb<-df(southboston)
se<-df(southend)
wb<-df(westroxbury)

qos_neighborhood<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)

table(qos2014$CASE_STATUS)

#by hours
qos_neighborhood14<- mutate(qos_neighborhood, time_consume = time_consume/60, consistency = consistency/60)

write.csv(qos_neighborhood14,"qos2014_neighborhood_hour.csv")

#####################################################################Department

table(qos2014$SUBJECT)

qos2013<- qos[grep("/2013",qos$OPEN_DT),]
table(qos2013$SUBJECT)



####loop
AB<-subset(department,department$neighborhood=="Allston / Brighton")
backbay<-subset(department,department$neighborhood=="Back Bay")
beaconhill<-subset(department,department$neighborhood=="Beacon Hill")
charlestown<-subset(department,department$neighborhood=="Charlestown")
dorchester<-subset(department,department$neighborhood=="Dorchester")
downtown<-subset(department,department$neighborhood=="Downtown / Financial District")
eastboston<-subset(department,department$neighborhood=="East Boston")
fkal<-subset(department,department$neighborhood=="Fenway / Kenmore / Audubon Circle / Longwood") 
hydepark<-subset(department,department$neighborhood=="Hyde Park")
jamaicaplain<-subset(department,department$neighborhood=="Jamaica Plain")
mattapan<-subset(department,department$neighborhood=="Mattapan")
missionhill<-subset(department,department$neighborhood=="Mission Hill")
roslindale<-subset(department,department$neighborhood=="Roslindale")
roxbury<-subset(department,department$neighborhood=="Roxbury")
southboston<-subset(department,department$neighborhood=="South Boston / South Boston Waterfront")
southend<-subset(department,department$neighborhood=="South End")
westroxbury<-subset(department,department$neighborhood=="West Roxbury")

ab<-df(AB)
bb<-df(backbay)
bh<-df(beaconhill)
ct<-df(charlestown)
dc<-df(dorchester)
dt<-df(downtown)
eb<-df(eastboston)
fk<-df(fkal)
hp<-df(hydepark)
jc<-df(jamaicaplain)
mp<-df(mattapan)
mh<-df(missionhill)
rl<-df(roslindale)
rb<-df(roxbury)
sb<-df(southboston)
se<-df(southend)
wb<-df(westroxbury)

################loop
#for"Public Works Department"
department<-subset(qos2014,qos2014$SUBJECT=="Public Works Department")
PWD2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
PWD2014$SUBJECT<-rep("Public Works Department",17)

#for Boston Public School
department<-subset(qos2014,qos2014$SUBJECT=="Boston Public School")
BPS2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
BPS2014$SUBJECT<-rep("Boston Public School",17)

#for boston water & sever commission
department<-subset(qos2014,qos2014$SUBJECT=="Boston Water & Sewer Commission")
BWSC2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
BWSC2014$SUBJECT<-rep("Boston Water & Sewer Commission",17)

#for inspectional services
department<-subset(qos2014,qos2014$SUBJECT=="Inspectional Services")
IS2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
IS2014$SUBJECT<-rep("Inspectional Services",17)

#for mayor's 24 hour hotline
department<-subset(qos2014,qos2014$SUBJECT=="Mayor's 24 Hour Hotline")
Mayor2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
Mayor2014$SUBJECT<-rep("Mayor's 24 Hour Hotline",17)

#for Parks &Recreation Department
department<-subset(qos2014,qos2014$SUBJECT=="Parks & Recreation Department")
PRD2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
PRD2014$SUBJECT<-rep("Parks & Recreation Department",17)

#for property management
department<-subset(qos2014,qos2014$SUBJECT=="Property Management")
PM2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
PM2014$SUBJECT<-rep("Property Management",17)

#for transportation
department<-subset(qos2014,qos2014$SUBJECT=="Transportation - Traffic Division")
Tra2014<-rbind(ab,bb,bh,ct,dc,dt,eb,fk,hp,jc,mp,mh,rl,rb,sb,se,wb)
Tra2014$SUBJECT<-rep("Transportation - Traffic Division",17)

qos_department_2014<-rbind(PWD2014,BPS2014,BWSC2014,IS2014,Mayor2014,PRD2014,PM2014,Tra2014)
#by hours
qos_department14<- mutate(qos_department_2014, time_consume = time_consume/60, consistency = consistency/60)

write.csv(qos_department14,"qos2014_department_hour.csv")

max(qos2014$CmO)

#EDA
library(ggplot2)
#Public work department
ggplot(subset(qos_department_2014,qos_department_2014$SUBJECT=="Public Works Department"))+geom_bar(aes(x=name,y=time_consume),stat="identity",fill=heat.colors(17))+xlab("Neighbourhood")+ylab("Average Time_consume/Day")+labs(title="Public Work Department")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$SUBJECT=="Public Works Department"))+geom_bar(aes(x=name,y=consistency),stat="identity",fill=heat.colors(17))+xlab("Neighbourhood")+ylab("Consistency/Day")+labs(title="Public Work Department")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$SUBJECT=="Public Works Department"))+geom_bar(aes(x=name,y=efficacy),stat="identity",fill=heat.colors(17))+xlab("Neighbourhood")+ylab("Efficacy/Rate")+labs(title="Public Work Department")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$SUBJECT=="Public Works Department"))+geom_bar(aes(x=name,y=ontime_rate),stat="identity",fill=heat.colors(17))+xlab("Neighbourhood")+ylab("Ontime Rate")+labs(title="Public Work Department")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$SUBJECT=="Public Works Department"))+geom_bar(aes(x=name,y=request_N),stat="identity",fill=heat.colors(17))+xlab("Neighbourhood")+ylab("Request Number")+labs(title="Public Work Department")+coord_flip()+theme(text=element_text(size=25))

PWD<-subset(qos2014,qos2014$SUBJECT=="Public Works Department")
PWD$time_consume<-PWD$CmO/60/60
ggplot(data=PWD,aes(x=neighborhood,y=time_consume))+geom_boxplot(color=heat.colors(17))+xlab("Neighbourhood")+labs(title="Public Work Department")+theme(axis.text.x=element_text(angle=45,size=10))
summary(PWD$time_consume)

#Dorchester
ggplot(subset(qos_department_2014,qos_department_2014$name=="Dorchester"))+geom_bar(aes(x=SUBJECT,y=time_consume),stat="identity",fill=heat.colors(8))+xlab("Department")+ylab("Average Time_consume/Day")+labs(title="Dorchester")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$name=="Dorchester"))+geom_bar(aes(x=SUBJECT,y=consistency),stat="identity",fill=heat.colors(7))+xlab("Department")+ylab("Consistency/Day")+labs(title="Dorchester")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$name=="Dorchester"))+geom_bar(aes(x=SUBJECT,y=efficacy),stat="identity",fill=heat.colors(8))+xlab("Department")+ylab("Efficacy/Day")+labs(title="Dorchester")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$name=="Dorchester"))+geom_bar(aes(x=SUBJECT,y=ontime_rate),stat="identity",fill=heat.colors(8))+xlab("Department")+ylab("Ontime Rate")+labs(title="Dorchester")+coord_flip()+theme(text=element_text(size=25))

ggplot(subset(qos_department_2014,qos_department_2014$name=="Dorchester"))+geom_bar(aes(x=SUBJECT,y=request_N),stat="identity",fill=heat.colors(8))+xlab("Department")+ylab("Request Number")+labs(title="Dorchester")+coord_flip()+theme(text=element_text(size=25))

Dor<-subset(qos2014,qos2014$neighborhood=="Dorchester")
Dor$time_consume<-Dor$CmO/60/60
ggplot(data=Dor,aes(x=SUBJECT,y=time_consume))+geom_boxplot(color=heat.colors(8))+xlab("Department")+labs(title="Dorchester")+theme(axis.text.x=element_text(angle=45,size=10))
