# efficacy
efficacy <- read.csv("/Users/jiayuan/Documents/MA675/311/regression - simple & multiple/csv/efficacy.csv",header=T)
attach(efficacy)

# category
efficacy$age[age<=35] <- 0
efficacy$age[age>35] <- 1
efficacy$white[white<=0.6] <- 0
efficacy$white[white>0.6 & white<=0.8] <- 1
efficacy$white[white>0.8] <- 2
efficacy$black.african[black.african <= 0.1] <- 0
efficacy$black.african[black.african > 0.1 & black.african <= 0.2] <- 1
efficacy$black.african[black.african > 0.2] <- 2
efficacy$asian[asian <= 0.05] <- 0
efficacy$asian[asian > 0.05 & asian <= 0.1] <- 1
efficacy$asian[asian > 0.1] <- 2
efficacy$hispanic.latino[hispanic.latino <= 0.1] <- 0
efficacy$hispanic.latino[hispanic.latino > 0.1 & hispanic.latino <= 0.2] <- 1
efficacy$hispanic.latino[hispanic.latino > 0.2] <- 2

# lm - public works
summary(lm(Public.Works.Department ~ female)) #Multiple R-squared:  0.3372
summary(lm(Public.Works.Department ~ male)) #Multiple R-squared:  0.3372
summary(lm(Public.Works.Department ~ factor(efficacy$black.african))) #Multiple R-squared:  0.487
# lm - Boston.Public.School
summary(lm(Boston.Public.School ~ income)) #Multiple R-squared:  0.3189
# lm - Boston.Water...Sewer.Commission, too many NAs so I pass it.
# lm - Inspectional.Services, no significance
# lm - Mayor.s.24.Hour.Hotline, no significance
# lm - Parks...Recreation.Department
summary(lm(Parks...Recreation.Department ~ income)) #Multiple R-squared:  0.2421
# lm - Property.Management
summary(lm(Property.Management ~ density)) #Multiple R-squared:  0.3291
# lm - Transportation...Traffic.Division, no significance

# multiple lm
summary(lm(Public.Works.Department ~ female + male + factor(efficacy$black.african)))

#### without category
summary(lm(Transportation...Traffic.Division ~ hispanic.latino))
summary(lm(Inspectional.Services ~ black.african))

