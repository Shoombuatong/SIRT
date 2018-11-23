R2 <- function(y, equation, ... ){
  1 - (sum((y-predict(equation))^2)/sum((y-mean(y))^2))
}
rm2 <- function(y, x, ... ){  
  if ((R2(y,(lm(y ~ x)))) > R2(y,(lm(y ~ -1 + x)))) { 
    print(R2(y,(lm(y ~ x)))*( 1-(sqrt(R2(y,(lm(y ~ x)))-R2(y,(lm(y ~ -1 + x)))))))
  } else { 
    print(R2(y,(lm(y ~ x))))
  } 
}
rm2.reverse <- function(y, x, ... ){
  print(R2(x,(lm(x ~ y)))*( 1-(sqrt(R2(x,(lm(x ~ y)))-R2(x,(lm(x ~ -1 + y)))))))
}
average.rm2 <- function(y, x, ... ){
  if ((R2(y,(lm(y ~ x)))) > R2(y,(lm(y ~ -1 + x)))) {
    print(((R2(y,(lm(y ~ x)))*( 1-(sqrt(R2(y,(lm(y ~ x)))-R2(y,(lm(y ~ -1 + x))))))+ R2(x,(lm(x ~ y)))*( 1-(sqrt(R2(x,(lm(x ~ y)))-R2(x,(lm(x ~ -1 + y))))))))/2)
  } else { 
    print(((R2(y,(lm(y ~ x))))  + (R2(x,(lm(x ~ y)))*( 1-(sqrt(R2(x,(lm(x ~ y)))-R2(x,(lm(x ~ -1 + y)))))))  )/2)
  }
}
delta.rm2 <- function(y, x, ... ){
  if ((R2(y,(lm(y ~ x)))) > R2(y,(lm(y ~ -1 + x)))) {
    print(abs((R2(y,(lm(y ~ x)))*( 1-(sqrt(R2(y,(lm(y ~ x)))-R2(y,(lm(y ~ -1 + x))))))  -  R2(x,(lm(x ~ y)))*( 1-(sqrt(R2(x,(lm(x ~ y)))-R2(x,(lm(x ~ -1 + y)))))))))
  } else { 
    print(abs((R2(y,(lm(y ~ x)))) - (R2(x,(lm(x ~ y)))*( 1-(sqrt(R2(x,(lm(x ~ y)))-R2(x,(lm(x ~ -1 + y)))))))  ))
  }
}


setwd('D:\\Data for R\\Large-scale Soil QSAR\\Currently results\\Pred vs Actual')

D =  read.csv("resultTS_MLR.csv", header = TRUE) 

average.rm2(D[,1], D[,2])
delta.rm2(D[,1], D[,2])
