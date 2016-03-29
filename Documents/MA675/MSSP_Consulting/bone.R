Flexion_513 <- read.csv("Flexion_513.csv",header=T)
Flexion_528 <- read.csv("Flexion_528.csv",header=T)
Union_581 <- read.csv("Union_581.csv",header=T)

# Flexion_513
attach(Flexion_513)

matrix.diff513 <- matrix(rep(0,19944), nrow=2493)
i <- 1
k <- 0
while(i < 24){
  diff513 <- Flexion_513[,i+1] - Flexion_513[,i+2]
  k <- k+1
  matrix.diff513[,k] <- diff513
  hist(diff513, main = "Flexion_513")
  i <- i+3
}

df513 <- data.frame(matrix.diff513)

mean(df513[,1])
median(df513[,2])
mean(df513[,3])
mean(df513[,4])
mean(df513[,5])
median(df513[,6])
mean(df513[,7])
mean(df513[,8])
# the 2nd is closest to 0 one

boxplot(df513[,1], df513[,2], df513[,3], df513[,4], df513[,5], df513[,6], df513[,7], df513[,8])

# Flexion_528
matrix.diff528 <- matrix(rep(0,15600), nrow=1950)
i <- 1
k <- 0
while(i < 24){
  diff528 <- Flexion_528[,i+1] - Flexion_528[,i+2]
  k <- k+1
  matrix.diff528[,k] <- diff528
  hist(diff528, main = "Flexion_528")
  i <- i+3
}
df528 <- data.frame(matrix.diff528[1:1897,])

median(df528[,1])
mean(df528[,2])
mean(df528[,3])
mean(df528[,4])
mean(df528[,5])
median(df528[,6])
mean(df528[,7])
mean(df528[,8])
# the 2nd is the closest to 0 one
boxplot(df528[,1], df528[,2], df528[,3], df528[,4], df528[,5], df528[,6], df528[,7], df528[,8])

# Union_581
matrix.diff581 <- matrix(rep(0,15512), nrow=1939)
i <- 1
k <- 0
while(i < 24){
  diff581 <- Union_581[,i+1] - Union_581[,i+2]
  k <- k+1
  matrix.diff581[,k] <- diff581
  hist(diff581, main = "Union_581")
  i <- i+3
}

df581 <- data.frame(matrix.diff581)

mean(df581[,1])
mean(df581[,2])
mean(df581[,3])
mean(df581[,4])
mean(df581[,5])
mean(df581[,6])
mean(df581[,7])
mean(df581[,8])

# all of them are quite close to each other, the 2nd and 7th one seems to be even better

boxplot(df581[,1], df581[,2], df581[,3], df581[,4], df581[,5], df581[,6], df581[,7], df581[,8])


############################################
s513 <- read.csv("s513.csv",header=T)
attach(s513)

# trail 1
model <- aov(difference ~ type + method + type:method, data = s513)
summary(model)

# trail 2
model<-aov(difference ~ type*method)
summary(model)

# trail 3
anova(lm(difference ~ type*method, s513))

# trail 4
inter <- type*method
