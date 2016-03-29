library(lubridate)
library(plyr)
QOS <- read.csv("qos.csv", header = TRUE, stringsAsFactors = F)
library("data.table")
qos <- data.table(QOS)

library(lubridate)

OPEN_DT <- as.POSIXct(strptime(qos$OPEN_DT, format = "%m/%d/%Y %I:%M:%S %p")) 
TARGET_DT <- as.POSIXct(strptime(qos$TARGET_DT, format = "%m/%d/%Y %I:%M:%S %p")) 
CLOSED_DT <- as.POSIXct(strptime(qos$CLOSED_DT, format = "%m/%d/%Y %I:%M:%S %p")) 


# we calculate the difference in minutes' unit
# I attached CmO, TmO, TmC as new columns in the dataset
qos$CmO <- difftime(CLOSED_DT, OPEN_DT, units = "secs")
qos$TmO <- difftime(TARGET_DT, OPEN_DT, units = "secs")
qos$TmC <- difftime(TARGET_DT, CLOSED_DT, units = "secs")
head(qos$CmO, 10)
head(qos$TmO, 10)
head(qos$TmC, 10)
cattach(qos)


# for TmC>=0, ontime status should be 'ontime', otherwise 'overdue'
OnTime_Status[which(TmC<0)] <- c("OVERDUE")
OnTime_Status[which(TmC>0 & TmC==0)] <- c("ONTIME")



# transform type of difftime into numeric
qos$CmO <- as.numeric(qos$CmO, units="mins")
class(qos$CmO)
qos$TmO <- as.numeric(qos$TmO, units="mins")
class(qos$TmO)
qos$TmC <- as.numeric(qos$TmC, units="mins")
class(qos$TmC)
data2 = subset(qos, qos$neighborhood != "")

#neighborhood
data2$neighborhood[(data2$neighborhood == "Allston")|(data2$neighborhood == "Brighton")] <- "Allston / Brighton"
data2$neighborhood[(data2$neighborhood == "Greater Mattapan")] <- "Mattapan"
data2$neighborhood[(data2$neighborhood == "South Boston")] <- "South Boston / South Boston Waterfront"
data2 <- subset(data2, (data2$neighborhood != "Chestnut Hill"))
#boston division
data2$neighborhood[(data2$neighborhood == "Boston")&(data2$LONGITUDE >= -71.050668)] <- "South Boston / South Boston Waterfront"
data2$neighborhood[(data2$neighborhood == "Boston")&(data2$LONGITUDE < -71.050668)&(data2$LONGITUDE >= -71.061628)] <- "Downtown / Financial District"
data2$neighborhood[(data2$neighborhood == "Boston")&(data2$LONGITUDE < -71.061628)&(data2$LONGITUDE >= -71.071893)] <- "Beacon Hill"
data2$neighborhood[(data2$neighborhood == "Boston")&(data2$LONGITUDE < -71.071893)&(data2$LONGITUDE >= -71.087847)] <- "Back Bay"
data2$neighborhood[(data2$neighborhood == "Boston")&(data2$LONGITUDE < -71.087847)&(data2$LONGITUDE >= -71.114501)] <- "Fenway / Kenmore / Audubon Circle / Longwood"
data2$neighborhood[(data2$neighborhood == "Boston")&(data2$LONGITUDE < -71.114501)] <- "Allston / Brighton"

#check data2
table(data2$neighborhood)

