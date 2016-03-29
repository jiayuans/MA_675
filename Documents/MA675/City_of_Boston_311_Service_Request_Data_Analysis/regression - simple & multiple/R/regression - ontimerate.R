# ontime
ontime <- read.csv("/Users/jiayuan/Documents/MA675/311/regression - simple & multiple/csv/ontime_rate.csv",header=T)
attach(ontime)

# category
ontime$age[age<=35] <- 0
ontime$age[age>35] <- 1
ontime$white[white<=0.6] <- 0
ontime$white[white>0.6 & white<=0.8] <- 1
ontime$white[white>0.8] <- 2
ontime$black.african[black.african <= 0.1] <- 0
ontime$black.african[black.african > 0.1 & black.african <= 0.2] <- 1
ontime$black.african[black.african > 0.2] <- 2
ontime$asian[asian <= 0.05] <- 0
ontime$asian[asian > 0.05 & asian <= 0.1] <- 1
ontime$asian[asian > 0.1] <- 2
ontime$hispanic.latino[hispanic.latino <= 0.1] <- 0
ontime$hispanic.latino[hispanic.latino > 0.1 & hispanic.latino <= 0.2] <- 1
ontime$hispanic.latino[hispanic.latino > 0.2] <- 2

# lm - public works
summary(lm(Public.Works.Department ~ area)) #Multiple R-squared:  0.2507
summary(lm(Public.Works.Department ~ density)) #Multiple R-squared:  0.3361
summary(lm(Public.Works.Department ~ age.over.18)) #Multiple R-squared:  0.4902
summary(lm(Public.Works.Department ~ factor(ontime$white))) #Multiple R-squared:  0.4847
summary(lm(Public.Works.Department ~ factor(ontime$black.african))) #Multiple R-squared:  0.6963
summary(lm(Public.Works.Department ~ factor(ontime$hispanic.latino))) #Multiple R-squared:  0.4321
# lm - Boston.Public.School, no significance
# lm - Boston.Water...Sewer.Commission, too many NAs so I pass it.
# lm - Inspectional.Services
summary(lm(Inspectional.Services ~ factor(ontime$white))) #Multiple R-squared:  0.5074
# lm - Mayor.s.24.Hour.Hotline, no significance
# lm - Parks...Recreation.Department
summary(lm(Parks...Recreation.Department ~ income)) #Multiple R-squared:  0.239
# lm - Property.Management
summary(lm(Property.Management ~ density)) #Multiple R-squared:  0.4383
summary(lm(Property.Management ~ age.over.18)) #Multiple R-squared:  0.3064
summary(lm(Property.Management ~ factor(ontime$age))) #Multiple R-squared:  0.3365
summary(lm(Property.Management ~ factor(ontime$asian))) #Multiple R-squared:  0.2844
# lm - Transportation...Traffic.Division, no significance

# multiple lm
summary(lm(Public.Works.Department ~ area + density + age.over.18 + factor(ontime$white) 
           + factor(ontime$black.african) + factor(ontime$hispanic.latino)))

summary(lm(Property.Management ~ factor(ontime$age) + factor(ontime$asian) 
           + age.over.18*factor(ontime$age)))
# interaction significant, density not significant anymore

