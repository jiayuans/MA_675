Flexion_513 <- read.csv("Flexion_513.csv",header=T)
Union_581 <- read.csv("Union_581.csv",header=T)

# Flexion_513
par(mfrow=c(3,3))
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
median(df513[,2]) # the 2nd is closest to 0 one
mean(df513[,3])
mean(df513[,4])
mean(df513[,5])
median(df513[,6])
mean(df513[,7])
mean(df513[,8])

# Union_581
par(mfrow=c(3,3))
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

flexion_data <- read.csv("Flexion.csv",header = T)
union_data <- read.csv("uni.csv",header = T)
names(flexion_data)
flexion.difference <- matrix(rep(0,279216),nrow=2493)
i <- 1
j <- 1
flexion.mean <- NULL
flexion.se <- NULL
while (i < 225) {
  flexion.difference[,j] <- flexion_data[,i]-flexion_data[,i+1]
  flexion.mean[j] <- mean(flexion.difference[,j],na.rm = T)
  flexion.se[j] <- sum((flexion.difference[,j])^2,na.rm = T)
  i <- i+2
  j <- j+1
}  
flexion.mean # mean difference for Flexion group
flexion.se # squared error for Flexion group

union.difference <- matrix(rep(0,320096),nrow=2858)
i <- 1
j <- 1
union.mean <- NULL
union.se <- NULL
while (i < 225) {
  union.difference[,j] <- union_data[,i]-union_data[,i+1]
  union.mean[j] <- mean(union.difference[,j],na.rm = T)
  union.se[j] <- sum((union.difference[,j])^2,na.rm = T)
  i <- i+2
  j <- j+1
}  
union.mean # mean difference for Union group
union.se # squared error for Union group

flexion.type <- c(rep(c(rep("CF",4),rep("vM",4)), 14))
flexion.method <- c(rep(c("Idealized","Experimental","Specific","Generic"),28))
flexion.data.mean <- cbind(flexion.type, flexion.method, flexion.mean )
flexion.data.se <- cbind(flexion.type, flexion.method, flexion.se)

# Flexion: influence of types and methods on mean difference
flexion.mean.fit <- aov(flexion.mean ~ flexion.type*flexion.method)
summary(flexion.mean.fit)
#Df Sum Sq Mean Sq F value  Pr(>F)   
#flexion.type                  1 0.0639 0.06391  10.684 0.00147 **
#  flexion.method                3 0.0573 0.01912   3.196 0.02657 * 
#  flexion.type:flexion.method   3 0.0067 0.00223   0.373 0.77284   
#Residuals                   104 0.6221 0.00598                               

# Flexion: influence of types and methods on squared error
flexion.se.fit <- aov(flexion.se ~ flexion.type*flexion.method)
summary(flexion.se.fit)
#Df  Sum Sq Mean Sq F value Pr(>F)  
#flexion.type                  1   75975   75975   5.108 0.0259 *
#  flexion.method                3   18659    6220   0.418 0.7403  
#flexion.type:flexion.method   3   11397    3799   0.255 0.8573  
#Residuals                   104 1546770   14873                               

type.union <- c(rep(c(rep("CF",4),rep("vM",4)), 14))
union.method <- c(rep(c("Idealized","Experimental","Specific","Generic"),28))
union.data.mean <- cbind(type.union, union.method, union.mean )
union.data.se <- cbind(type.union, union.method, union.se )

# Union: influence of types and methods on mean difference
union.mean.fit <- aov(union.mean ~ type.union*union.method)
summary(union.mean.fit)
#Df Sum Sq Mean Sq F value Pr(>F)
#type.union                1  0.003 0.00306   0.094  0.760
#union.method              3  0.015 0.00505   0.156  0.926
#type.union:union.method   3  0.023 0.00779   0.240  0.868
#Residuals               104  3.378 0.03248               

# Union: influence of types and methods on squared error
union.se.fit <- aov(union.se ~ type.union*union.method)
summary(union.se.fit)
#Df  Sum Sq Mean Sq F value Pr(>F)
#type.union                1    9964    9964   0.263  0.609
#union.method              3  141505   47168   1.243  0.298
#type.union:union.method   3  159942   53314   1.405  0.245
#Residuals               104 3945671   37939               

flexion.mean.Tukey <- TukeyHSD(flexion.mean.fit, conf.level=0.95)
flexion.mean.Tukey
#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = flexion.mean ~ flexion.type * flexion.method)
#
#$flexion.type
#diff         lwr         upr     p adj
#vM-CF -0.0477743 -0.07675817 -0.01879043 0.0014652
#
#$flexion.method
#diff         lwr          upr     p adj
#Generic-Experimental   -0.055162356 -0.10913295 -0.001191766 0.0431667
#Idealized-Experimental -0.048569144 -0.10253973  0.005401445 0.0935717
#Specific-Experimental  -0.052206779 -0.10617737  0.001763811 0.0617960
#Idealized-Generic       0.006593212 -0.04737738  0.060563801 0.9887094
#Specific-Generic        0.002955577 -0.05101501  0.056926167 0.9989507
#Specific-Idealized     -0.003637634 -0.05760822  0.050332955 0.9980516
plot(flexion.mean.Tukey)

