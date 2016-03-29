data1 <- read.csv("513.csv", header = T)

a=8
b=2493
y.m=matrix(nrow=a,ncol=b)
y.m[1,]=data1[,1]
y.m[2,]=data1[,2]
y.m[3,]=data1[,3]
y.m[4,]=data1[,4]
y.m[5,]=data1[,5]
y.m[6,]=data1[,6]
y.m[7,]=data1[,7]
y.m[8,]=data1[,8]

mean(y.m[1,])

y=as.vector(t(y.m))
A=as.factor(rep(1:a,each=b))
data=data.frame(A,y)
lm.1=lm(y~A)
summary.aov(lm.1)


a=2
b=4
x.m=matrix(nrow=a,ncol=b)
x.m[1,]=c(mean(data1[,1]),mean(data1[,2]),mean(data1[,3]),mean(data1[,4]))
x.m[2,]=c(mean(data1[,5]),mean(data1[,6]),mean(data1[,7]),mean(data1[,8]))
x=as.vector(t(x.m))
A=as.factor(rep(1:a,each=b))
data=data.frame(A,x)
lm.1=lm(x~A)
summary.aov(lm.1)


r = c(x.m)
f = c("TypeA", "TypeB")
k=2
n=4
tm = gl(k, 1, n*k, factor(f))
blk = gl(n, k, k*n)
av = aov(r ~ tm + blk + tm*blk)
summary(av)


