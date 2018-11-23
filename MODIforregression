setwd('D:\\MODI index')

Dat <- read.csv("Soil2186_Top30 for making webserver.csv", header = TRUE) 
n = ncol(Dat)
m= n-1
AA <- Dat[,1:m]

d1 <- dist(AA, upper=TRUE, diag=TRUE, method = "euclidean")
nd2 = scale(d1)
MOBI3knn <- matrix(nrow = nrow(Dat), ncol =3)
MOBI5knn <- matrix(nrow = nrow(Dat), ncol =5)
MOBI3knn_aver <- matrix(nrow = nrow(Dat), ncol =1)
MOBI5knn_aver <- matrix(nrow = nrow(Dat), ncol =1)

for (i in 1:nrow(Dat)){
MOBI3knn[i,] <- Dat[order(nd2[i,]),][2:4,n]
MOBI5knn[i,] <- Dat[order(nd2[i,]),][2:6,n]
}

for (i in 1:nrow(Dat)){
MOBI3knn_aver[i,] <- mean(MOBI3knn[i,])
MOBI5knn_aver[i,] <- mean(MOBI5knn[i,])
}

result = data.frame(MOBI3knn_aver,MOBI5knn_aver,Dat[,n])

MODI_q2_3knn = cor(result[1],result[3])*cor(result[1],result[3])
MODI_q2_5knn = cor(result[2],result[3])*cor(result[2],result[3])

data.frame(MODI_q2_3knn,MODI_q2_5knn)

##### MODI (3knn) > 0.46 and MODI (5knn) > 0.47
