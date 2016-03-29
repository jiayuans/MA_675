setwd("/Users/jiayuan/Documents/MA675/311")
service <- read.csv("311__Service_Requests-2.csv", header=TRUE)
require("data.table") 
qos <- data.table(service)

require(lubridate)
qos$OPEN_DT <- as.POSIXct(qos$OPEN_DT, format = "%m/%d/%Y %I:%M:%S %p") 
qos$TARGET_DT <- as.POSIXct(qos$TARGET_DT, format = "%m/%d/%Y %I:%M:%S %p") 
qos$CLOSED_DT <- as.POSIXct(qos$CLOSED_DT, format = "%m/%d/%Y %I:%M:%S %p") 

time <- qos$TARGET_DT - qos$CLOSED_DT

summary(qos$OnTime_Status[which(time>0)])
#   ONTIME OVERDUE 
#0  384554       0 
summary(qos$OnTime_Status[which(time<0)])
#         ONTIME OVERDUE 
#476       0  114764 
summary(qos$OnTime_Status[which(time==0)])
#         ONTIME OVERDUE 
#0       0       1 
summary(qos$OnTime_Status[which(is.na(time)==T)])
#         ONTIME OVERDUE 
#105  163887   34270 
summary(qos$OnTime_Status[which(is.na(qos$TARGET_DT)==T)])
#         ONTIME OVERDUE 
#0  156759       0 
summary(qos$OnTime_Status[which(is.na(qos$CLOSED_DT)==T)])
#         ONTIME OVERDUE 
#105   19899   34270 
summary(qos$OnTime_Status[which(is.na(qos$CLOSED_DT)==T & is.na(qos$TARGET_DT)==T)])
#         ONTIME OVERDUE 
#0   12771       0 

summary(qos$OnTime_Status)
#         ONTIME OVERDUE 
#581  548441  149035  

##time<0, overdue
qos$OnTime_Status[which(time<0)] <- c("OVERDUE")
##time>0, ontime
#OnTime_Status[which(time>0)] <- c("ONTIME")
##time=0, ontime
qos$OnTime_Status[which(time==0)] <- c("ONTIME")

##time=NA, 
#TARGET_DT=NA, follow OnTime_Status
#OnTime_Status[which(is.na(TARGET_DT)==TRUE)] = c("ONTIME")

#CLOSED_DT=NA, follow OnTime_Status, drop NA
delete <- which(qos$OnTime_Status=="")
qos.clean=qos[-c(delete),]

qos.clean=qos.clean[-c(which(qos.clean$OPEN_DT>qos.clean$CLOSED_DT)),]
qos.clean=qos.clean[-c(which(qos.clean$OPEN_DT>qos.clean$TARGET_DT)),]

attach(qos.clean)
length(which(OnTime_Status=="")) #0







setwd("/Users/jiayuan/Documents/MA675/311")
service <- read.csv("311__Service_Requests-2.csv", header=TRUE)
require("data.table") 
qos <- data.table(service)

require(lubridate)
qos$OPEN_DT <- as.POSIXct(qos$OPEN_DT, format = "%m/%d/%Y %I:%M:%S %p") 
qos$TARGET_DT <- as.POSIXct(qos$TARGET_DT, format = "%m/%d/%Y %I:%M:%S %p") 
qos$CLOSED_DT <- as.POSIXct(qos$CLOSED_DT, format = "%m/%d/%Y %I:%M:%S %p") 

time <- qos$TARGET_DT - qos$CLOSED_DT
qos$OnTime_Status[which(time<0)] <- c("OVERDUE")
qos$OnTime_Status[which(time==0)] <- c("ONTIME")
delete <- which(qos$OnTime_Status=="")
qos.clean=qos[-c(delete),]

attach(qos.clean)
#drop the values OPEN_DT>CLOSED_DT
qos.clean=qos.clean[-c(which(qos.clean$OPEN_DT>qos.clean$CLOSED_DT)),]
qos.clean=qos.clean[-c(which(qos.clean$OPEN_DT>qos.clean$TARGET_DT)),]

#Check:
length(which(qos.clean$OnTime_Status=="")) #0,no missing values 
#qos.clean: the final observatiions are 69


