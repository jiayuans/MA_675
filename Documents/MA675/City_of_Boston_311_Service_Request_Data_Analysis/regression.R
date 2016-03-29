qos2014<- read.csv("F:/MSSP/2015FALL/MA 881/API/qos2014_neighborhood_hour.csv")
attach(qos2014)
View(qos2014)

#efficacy
regout1<-lm(efficacy~ontime_rate) # p<0.001
summary(regout1)

regout2<-lm(efficacy~time_consume) #p<0.001
summary(regout2)

regout3<-lm(efficacy~request_N) #not significant
summary(regout3)

regout4<-lm(efficacy~population) # not significant
summary(regout4)

regout5<-lm(efficacy~area) #not significant
summary(regout5)

regout6<-lm(efficacy~density) #p =0.0155
summary(regout6)

regout7<-lm(efficacy~age) #p=0.00742
summary(regout7)

regout8<-lm(efficacy~income) # not significant
summary(regout8)

regout9<-lm(efficacy~male) # p=0.02715
summary(regout9)

regout10<-lm(efficacy~female) #p=0.0271
summary(regout10)

regout11<-lm(efficacy~age.over.18) #p=0.009306
summary(regout11)

regout12<-lm(efficacy~white) #not significant
summary(regout12)

regout13<-lm(efficacy~black.african) #not
summary(regout13)

regout14<-lm(efficacy~asian) #0.00485
summary(regout14)

regout15<-lm(efficacy~hispanic.latino) #not
summary(regout15)

regout16<-lm(efficacy~other.race) #not
summary(regout16)

regout17<-lm(efficacy~consistency) #not
summary(regout17)

#consistency

regout21<-lm(consistency~ontime_rate) #not significant
summary(regout21)

regout22<-lm(consistency~time_consume) #not
summary(regout22)

regout23<-lm(consistency~request_N) #not significant
summary(regout23)

regout24<-lm(consistency~population) # not significant
summary(regout24)

regout25<-lm(consistency~area) #not significant
summary(regout25)

regout26<-lm(consistency~density) #not significant
summary(regout26)

regout27<-lm(consistency~age) #not significant
summary(regout27)

regout28<-lm(consistency~income) #not significant
summary(regout28)

regout29<-lm(consistency~male) # not significant
summary(regout29)

regout210<-lm(consistency~female) #not
summary(regout210)

regout211<-lm(consistency~age.over.18) #not
summary(regout211)

regout212<-lm(consistency~white) #not
summary(regout212)

regout213<-lm(consistency~black.african) # not
summary(regout213)

regout214<-lm(consistency~asian) #not
summary(regout214)

regout215<-lm(consistency~hispanic.latino) #not
summary(regout215)

regout216<-lm(consistency~other.race) #not
summary(regout216)

regout217<-lm(consistency~efficacy) #not
summary(regout217)

#ontime_rate

regout31<-lm(ontime_rate~consistency) #not significant
summary(regout31)

regout32<-lm(ontime_rate~time_consume) #p=0.0116
summary(regout32)

regout33<-lm(ontime_rate~request_N) #not significant
summary(regout33)

regout34<-lm(ontime_rate~population) # not significant
summary(regout34)

regout35<-lm(ontime_rate~area) #p=0.0355<0.05
summary(regout35)

regout36<-lm(ontime_rate~density) #p=0.031
summary(regout36)

regout37<-lm(ontime_rate~age) #not significant
summary(regout37)

regout38<-lm(ontime_rate~income) #not significant
summary(regout38)

regout39<-lm(ontime_rate~male) # not significant
summary(regout39)

regout310<-lm(ontime_rate~female) #not
summary(regout310)

regout311<-lm(ontime_rate~age.over.18) #p=0.0152
summary(regout311)

regout312<-lm(ontime_rate~white) #not
summary(regout312)

regout313<-lm(ontime_rate~black.african) # not
summary(regout313)

regout314<-lm(ontime_rate~asian) #not
summary(regout314)

regout315<-lm(ontime_rate~hispanic.latino) #not
summary(regout315)

regout316<-lm(ontime_rate~other.race) #not
summary(regout316)

regout317<-lm(ontime_rate~efficacy) #p<0.001
summary(regout317)

#request number

regout41<-lm(request_N~consistency) #not significant
summary(regout41)

regout42<-lm(request_N~time_consume) #not 
summary(regout42)

regout43<-lm(request_N~ontime_rate) #not significant
summary(regout43)

regout44<-lm(request_N~population) #p<0.001
summary(regout44)

regout45<-lm(request_N~area) #p<0.001
summary(regout45)

regout46<-lm(request_N~density) #not
summary(regout46)

regout47<-lm(request_N~age) #not significant
summary(regout47)

regout48<-lm(request_N~income) #not significant
summary(regout48)

regout49<-lm(request_N~male) # not significant
summary(regout49)

regout410<-lm(request_N~female) #not
summary(regout410)

regout411<-lm(request_N~age.over.18) #not
summary(regout411)

regout412<-lm(request_N~white) #not
summary(regout412)

regout413<-lm(request_N~black.african) # not
summary(regout413)

regout414<-lm(request_N~asian) #not
summary(regout414)

regout415<-lm(request_N~hispanic.latino) #not
summary(regout415)

regout416<-lm(request_N~other.race) #not
summary(regout416)

regout417<-lm(request_N~efficacy) #not
summary(regout417)

regout418<-lm(request_N~area+population) #population significant, area not
summary(regout418)

#time consume

regout51<-lm(time_consume~consistency) #not
summary(regout51)

regout52<-lm(time_consume~request_N) #not 
summary(regout52)

regout53<-lm(time_consume~ontime_rate) #p=0.01156
summary(regout53)

regout54<-lm(time_consume~population) #not
summary(regout54)

regout55<-lm(time_consume~area) #not
summary(regout55)

regout56<-lm(time_consume~density) #p=0.01869
summary(regout56)

regout57<-lm(time_consume~age) #p=0.011973
summary(regout57)

regout58<-lm(time_consume~income) #not significant
summary(regout58)

regout59<-lm(time_consume~male) # not significant
summary(regout59)

regout510<-lm(time_consume~female) #not
summary(regout510)

regout511<-lm(time_consume~age.over.18) #p=0.00101
summary(regout511)

regout512<-lm(time_consume~white) #not
summary(regout512)

regout513<-lm(time_consume~black.african) # not
summary(regout513)

regout514<-lm(time_consume~asian) #p<0.001
summary(regout514)

regout515<-lm(time_consume~hispanic.latino) #not
summary(regout515)

regout516<-lm(time_consume~other.race) #not
summary(regout516)

regout517<-lm(time_consume~efficacy) #p<0.001
summary(regout517)

