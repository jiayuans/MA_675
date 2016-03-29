# timecons
timecons <- read.csv("time_consume.csv",header=T)
attach(timecons)

# category
timecons$age[age<=35] <- 0
timecons$age[age>35] <- 1
timecons$white[white<=0.6] <- 0
timecons$white[white>0.6 & white<=0.8] <- 1
timecons$white[white>0.8] <- 2
timecons$black.african[black.african <= 0.1] <- 0
timecons$black.african[black.african > 0.1 & black.african <= 0.2] <- 1
timecons$black.african[black.african > 0.2] <- 2
timecons$asian[asian <= 0.05] <- 0
timecons$asian[asian > 0.05 & asian <= 0.1] <- 1
timecons$asian[asian > 0.1] <- 2
timecons$hispanic.latino[hispanic.latino <= 0.1] <- 0
timecons$hispanic.latino[hispanic.latino > 0.1 & hispanic.latino <= 0.2] <- 1
timecons$hispanic.latino[hispanic.latino > 0.2] <- 2

# lm - public works
summary(lm(Public.Works.Department ~ male)) #Multiple R-squared:  0.3735
summary(lm(Public.Works.Department ~ female)) #Multiple R-squared:  0.3735
summary(lm(Public.Works.Department ~ factor(timecons$black.african))) #Multiple R-squared:  0.4547
# lm - Boston.Public.School
summary(lm(Boston.Public.School ~ male)) #Multiple R-squared:  0.3824
summary(lm(Boston.Public.School ~ female)) #Multiple R-squared:  0.3824
# lm - Boston.Water...Sewer.Commission, too many NAs so I pass it.
# lm - Inspectional.Services
summary(lm(Inspectional.Services ~ income)) #Multiple R-squared:  0.521
summary(lm(Inspectional.Services ~ factor(timecons$white))) #Multiple R-squared:  0.3219
summary(lm(Inspectional.Services ~ factor(timecons$black.african))) #Multiple R-squared:  0.2191
# lm - Mayor.s.24.Hour.Hotline, no significance
# lm - Parks...Recreation.Department
summary(lm(Parks...Recreation.Department ~ income)) #Multiple R-squared:  0.5932
summary(lm(Parks...Recreation.Department ~ factor(timecons$white))) #Multiple R-squared:  0.4185
# lm - Property.Management
summary(lm(Property.Management ~ density)) #Multiple R-squared:  0.3595
summary(lm(Property.Management ~ factor(timecons$age))) #Multiple R-squared:  0.2776
# lm - Transportation...Traffic.Division
summary(lm(Transportation...Traffic.Division ~ area)) #Multiple R-squared:  0.3398
summary(lm(Transportation...Traffic.Division ~ density)) #Multiple R-squared:  0.3329
summary(lm(Transportation...Traffic.Division ~ age.over.18)) #Multiple R-squared:  0.4026
summary(lm(Transportation...Traffic.Division ~ male)) #Multiple R-squared:  0.2334
summary(lm(Transportation...Traffic.Division ~ female)) #Multiple R-squared:  0.2334
summary(lm(Transportation...Traffic.Division ~ factor(timecons$black.african))) #Multiple R-squared:  0.3984

