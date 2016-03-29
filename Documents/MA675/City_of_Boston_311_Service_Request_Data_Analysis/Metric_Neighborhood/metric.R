library(lubridate)
library(plyr)
library("data.table")
qos<-read.csv("qos.csv",header = TRUE,stringsAsFactors = F)
qos <- data.table(qos)

qos2014<-qos[grep("/2014",qos$OPEN_DT),]
table(PWD2014$REASON)
table(PWD2014$TYPE)
table(qos2014$neighborhood)
#get rid of c-o na and efficacy2 na
qos2014<-subset(qos2014,is.na(qos2014$TmO)==F)
qos2014<-subset(qos2014,is.na(qos2014$CmO)==F)


qos2014$TmO<-as.numeric(qos2014$TmO)
qos2014$CmO<-as.numeric(qos2014$CmO)

#calculate x%
#x is the mean(CmO)/mean(TmO)

PWD2014<-subset(qos2014,qos2014$SUBJECT=="Public Works Department")

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
  average<-mean(dataframe$CmO)
  t<-mean(dataframe$TmO)
  consistency<-sd(dataframe$CmO)
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

write.csv(qos_neighborhood,"qos2014_neighborhood.csv")
