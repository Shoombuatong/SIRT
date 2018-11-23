#######set directory
setwd('D:\\Data for R\\Large-scale Soil QSAR')
library(RWeka)
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(kernlab)
library(e1071)
library(cluster) 
library(FactoMineR)
library(randomGLM)
library(corrplot)
library(C50)
library(nnet)
library(e1071)
library(GA)
library(cvTools) 
library(Metrics)
library(MASS)
library(plsdepot)

D =  read.csv("Soil2186_Top30-TRN.csv", header = TRUE) 
data= data.frame(D[,-ncol(D)],LogKoc = D[,ncol(D)])

######### Scrampling Y-variable 
Y = data[,ncol(data)]
newY <- matrix(nrow = length(Y), ncol = 100)

for (j in 1: 100){
   for (i in 1: length(Y)){
       id <- sample.int(nrow(data), size = nrow(data), replace = FALSE)
       newY[i,j] = Y[id[i]]
   }
}

#################### Results of Y-scrambling 
R2 <- matrix(nrow = 100, ncol = 1)
Q2 <- matrix(nrow = 100, ncol = 1)

#################### Results of Y-scrambling (MLR)
for ( y in 1:100){
internal= data.frame(data[,-ncol(data)],LogKoc = newY[,y])
#################################################
id <- sample(1:10,nrow(internal),replace=TRUE)
list <- 1:10
prediction <- data.frame()
testsetCopy <- data.frame()

for (h in 1:10){
  train <- subset(internal, id %in% list[-h])
  test <- subset(internal, id %in% c(h))
  my_pls1 = lm( LogKoc ~., data= data.frame(train))
  step <- stepAIC(my_pls1, direction="both"); 
  pred = as.matrix(predict(step,test[,-ncol(test)]))
  prediction <- rbind(prediction, pred)
  testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}
result <-  data.frame( prediction, testsetCopy)
Q2[y,] = cor(result[,1] ,result[,2] )*cor(result[,1] ,result[,2] )

###################Full data
my_pls1 =lm( LogKoc ~., data= data.frame(internal))
step <- stepAIC(my_pls1, direction="both"); 
pred <- predict(step,data.frame(internal))
R2[y,] = cor(pred,internal[,ncol(internal)])*cor(pred,internal[,ncol(internal)])
}
write.csv(round(data.frame(R2,Q2),4) , "Y-scrambling MLR.csv", row.names=FALSE, na="")
