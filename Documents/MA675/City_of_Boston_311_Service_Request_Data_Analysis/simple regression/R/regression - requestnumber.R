# requestnum
requestnum <- read.csv("request_number.csv",header=T)
attach(requestnum)

# category
requestnum$age[age<=35] <- 0
requestnum$age[age>35] <- 1
requestnum$white[white<=0.6] <- 0
requestnum$white[white>0.6 & white<=0.8] <- 1
requestnum$white[white>0.8] <- 2
requestnum$black.african[black.african <= 0.1] <- 0
requestnum$black.african[black.african > 0.1 & black.african <= 0.2] <- 1
requestnum$black.african[black.african > 0.2] <- 2
requestnum$asian[asian <= 0.05] <- 0
requestnum$asian[asian > 0.05 & asian <= 0.1] <- 1
requestnum$asian[asian > 0.1] <- 2
requestnum$hispanic.latino[hispanic.latino <= 0.1] <- 0
requestnum$hispanic.latino[hispanic.latino > 0.1 & hispanic.latino <= 0.2] <- 1
requestnum$hispanic.latino[hispanic.latino > 0.2] <- 2

# lm - public works
summary(lm(Public.Works.Department ~ population)) #Multiple R-squared:  0.6164
summary(lm(Public.Works.Department ~ area)) #Multiple R-squared:  0.3734
# lm - Boston.Public.School
summary(lm(Boston.Public.School ~ population)) #Multiple R-squared:  0.4888
summary(lm(Boston.Public.School ~ area)) #Multiple R-squared:  0.4286
summary(lm(Boston.Public.School ~ age.over.18)) #Multiple R-squared:  0.4423
summary(lm(Boston.Public.School ~ factor(requestnum$black.african))) #Multiple R-squared:  0.5089
# lm - Boston.Water...Sewer.Commission, too many NAs so I pass it.
# lm - Inspectional.Services
summary(lm(Inspectional.Services ~ population)) #Multiple R-squared:  0.7806
summary(lm(Inspectional.Services ~ area)) #Multiple R-squared:   0.45
# lm - Mayor.s.24.Hour.Hotline
summary(lm(Mayor.s.24.Hour.Hotline ~ population)) #Multiple R-squared:  0.4863
# lm - Parks...Recreation.Department
summary(lm(Parks...Recreation.Department ~ population)) #Multiple R-squared:  0.5227
summary(lm(Parks...Recreation.Department ~ area)) #Multiple R-squared:  0.7207
summary(lm(Parks...Recreation.Department ~ density)) #Multiple R-squared:  0.7207
summary(lm(Parks...Recreation.Department ~ age.over.18)) #Multiple R-squared:  0.3168
# lm - Property.Management
summary(lm(Property.Management ~ age.over.18)) #Multiple R-squared:  0.285
summary(lm(Property.Management ~ male)) #Multiple R-squared:  0.2788
summary(lm(Property.Management ~ female)) #Multiple R-squared:  0.2788
summary(lm(Property.Management ~ factor(requestnum$asian))) #Multiple R-squared:  0.4627
# lm - Transportation...Traffic.Division
summary(lm(Transportation...Traffic.Division ~ population)) #Multiple R-squared:  0.5603

