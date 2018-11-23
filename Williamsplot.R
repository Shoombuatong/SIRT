setwd('D:\\Data for R\\Large-scale Soil QSAR\\Old\\RF')

data = read.csv("Hat value.csv", header = TRUE)

par( mfrow = c(2,2 ),mai=c(0.4,0.4,0.3,0.3))

x = data[,1]
y = data[,2]
fit = lm(y~x,data = data)
hv <- as.data.frame(hatvalues(fit))
std.error = scale(data[,3])
william = data.frame(hv,std.error)
plot(william[,1], william[,2], main= "MLR",xlab = "." ,ylab = ".",col=ifelse(data [[13]]=="CV","seagreen3","darkorchid4"),pch=16, lwd =2,xlim=c(0,0.2))
abline(h=c(-3,3), col = 'blue')
abline(v=0.146,col = 'blue')

x = data[,4]
y = data[,5]
fit = lm(y~x)
hv <- as.data.frame(hatvalues(fit))
std.error = scale(data[,6])
william = data.frame(hv,std.error)
plot(william[,1], william[,2], main= "ANN",xlab = "." ,ylab = ".",col=ifelse(data [[13]]=="CV","seagreen3","darkorchid4"),pch=16, lwd =2,xlim=c(0,0.2))
abline(h=c(-3,3), col = 'blue')
abline(v=0.146,col = 'blue')

x = data[,7]
y = data[,8]
fit = lm(y~x)
hv <- as.data.frame(hatvalues(fit))
std.error = scale(data[,9])
william = data.frame(hv,std.error)
plot(william[,1], william[,2], main= "SVM",xlab = "." ,ylab = ".",col=ifelse(data [[13]]=="CV","seagreen3","darkorchid4"),pch=16, lwd =2,xlim=c(0,0.2))
abline(h=c(-3,3), col = 'blue')
abline(v=0.146,col = 'blue')


x = data[,10]
y = data[,11]
fit = lm(y~x)
hv <- as.data.frame(hatvalues(fit))
std.error = scale(data[,12])
william = data.frame(hv,std.error)
plot(william[,1], william[,2], main= "PLS",xlab = "." ,ylab = ".",col=ifelse(data [[13]]=="CV","seagreen3","darkorchid4"),pch=16, lwd =2,xlim=c(0,0.2))
abline(h=c(-3,3), col = 'blue')
abline(v=0.146,col = 'blue')
