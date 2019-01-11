#Bootstrapping practice
#challenge 1
data <- c( 1, 1, 2, 2, 4, 5, 7, 15, 30, 56)
median(data)
mean(data)
hist(data)


#challenge 2
data2 <- sample(data,10,replace = TRUE)
data2
mean(data2)
median(data2)
hist(data2)

#challenge 3
meanData <- c()
medianData<-c()
system.time(
for(i in 1:10000){
  x3 <- (sample(data,10,replace = TRUE))
  meanData[i] <- mean(x3)
  medianData[i] <- median(x3)
})
sampData
hist(meanData)
mean(meanData)
mean(medianData)


#challenge 4
quantile(meanData, prob = c(.025,.975))
t.test(data)


#cor
n=20
x=rnorm(n)
rho= -.5
y=rho*x+sqrt(1-rho^2)*rnorm(n)
x
y
cor(x,y)
rStar=c()
B = 10000
tTime <- Sys.time()
for(b in 1:B){
 sampleInd = sample(1:n,n,replace =TRUE)
  bootX = x[sampleInd]
  bootY = y[sampleInd]
  rStar[b] =  cor(bootX, bootY)
    
}
uTime <- Sys.time()
time<- uTime-tTime
time 

hist(rStar)
lines = quantile(rStar, prob = c(.025,.975))
abline(v=lines)
cor.test(x,y)

#######################################################

#bootstrapping practice for 6/25/18

library(boot)
library(bootstrap)

# compute abc intervals for the mean
#need stats explaination for results?

startTime<- Sys.time()
x <- rnorm(10)
theta <- function(p,x) {sum(p*x)/sum(p)}
results <- abcnon(x, theta)
# compute abc intervals for the correlation
x <- matrix(rnorm(20),ncol=2)
theta <- function(p, x)
{
  x1m <- sum(p * x[, 1])/sum(p)
  x2m <- sum(p * x[, 2])/sum(p)
  num <- sum(p * (x[, 1] - x1m) * (x[, 2] - x2m))
  den <- sqrt(sum(p * (x[, 1] - x1m)^2) *
                sum(p * (x[, 2] - x2m)^2))
  return(num/den)
}
results <- abcnon(x, theta)
results
stopTime<- Sys.time()
time <- stopTime - startTime
time

?abcnon
??abcnon
?bootstrap

n<-20
bootEx1 <- boot(1:n, 1, 100)
bootEx1

# practice with sample mean
a = c(15,20,38,43,51)

samplemean <- function(a, b) {
  return(mean(a[b]))
}
samplemean2 <- function(a,b){
  newA = a[b]
  return(sum((newA)/length(newA)))
}
sampCor <- function(xyFrame,rowsToSamp){
  cor(xyFrame[rowsToSamp,1],xyFrame[rowsToSamp,2])
}

n=1000
d= matrix(rnorm(n*2), nrow= n, ncol =2)
sampCor(d)

n=1000
x= rnorm(n)
y0 = rnorm(n)
rho = .5
y=rho*x+sqrt(1-rho^2)*y0
this = cbind(x,y)
sampCor(this)

#samplemean2(a,1:5)
x=d[,1]
y=d[,2]

SS1<-function(u){
  sum((u-mean(u))^2)
}
#create vector of speed

#x = rep(NA, 3)
#names(x) = c("sampCor", etc)

p=rnorm(10)
SS1(p)
var(p)
SS1(p)/(10-1)
S = 100
B = 10
ciMatrix = matrix(NA,nrow=S, ncol = 2)
system.time(
for(s in 1:S){

     bootObject = boot(this, samplemean2, R=B)
     #bootObject
     #ci = boot.ci(bootObject, type="basic") 
     ciMatrix[s,]= quantile(bootObject$t,c(.025,.975))
      
     
}
)
ciMatrix[1:3,]

##########################################
library(boot)
library(bootstrap)

#Challenges for 7/16 meeting
# BE CAREFUL DO NOT MOVE
#loop to use sampCor in S simulations

#sampCorVec
sampCorVec = c(sampCor,sampCor11,sampCor12,sampCor21,sampCor22,sampCorSuper)

sampCorVec[[3]]

#data
n=100
x= rnorm(n)
y0 = rnorm(n)
rho = .5
y=rho*x+sqrt(1-rho^2)*y0
data = cbind(x,y)
B = 10000
S=100

# fMax= 5
# for(f in 1:fMax){
#   sampCor
#   startTime<- Sys.time()
#   
#   bootObject = boot(data, sampCorVec[[1]], R=B)
#   
#   stopTime<- Sys.time()
#   time <- stopTime - startTime
#   
# }
# bootObject
# data
###############################################################################
S=10
timeCorMatrix = matrix(NA,nrow=S, ncol = 6)
colnames(timeCorMatrix, do.NULL = FALSE)
colnames(timeCorMatrix) <- c("sampCor","sampCor11","sampCor12","sampCor21","sampCor22","sampCorSuper")


for(s in 1:S){
  #s=1
  fMax= 6
  for(f in 1:fMax){
    startTime<- Sys.time()
  
    bootObject = boot(data, sampCorVec[[f]], R=B)
    
    stopTime<- Sys.time()
    time <- stopTime - startTime
    timeCorMatrix[s, f] = time
    
  }
  
  
  ciMatrix[s,]= quantile(bootObject$t,c(.025,.975))
  
  
  
}

ci95 <- function(x){
n = length(x)
se = sd(x)/sqrt(n)
return (qt(.975,df=n-1)*se)
}
 
  colSums(timeCorMatrix)
  apply(timeCorMatrix, 2, ci95)
  
####################################################################
  
#functions
  
  samplemean <- function(a, b) {
    return(mean(a[b]))
  }
  samplemean2 <- function(a,b){
    newA = a[b]
    return(sum((newA)/length(newA)))
  }
  
  sampCor <- function(xyFrame,rowsToSamp){
    cor(xyFrame[rowsToSamp,1],xyFrame[rowsToSamp,2])
  }
  
 #SampCor11
  sampCor11 <- function(xyFrame,rowsToSamp){
    rowsData = xyFrame[rowsToSamp,]
    x=rowsData[,1]
    y=rowsData[,2]
    spXY = SP1(rowsData)
    sqSxSy = sqrt(SS1(x)*SS1(y))
    return (spXY/sqSxSy)
  }
  
  sampCor11(data)
 
  
  #SampCor12
  sampCor12 <- function(xyFrame,rowsToSamp){
    rowsData = xyFrame[rowsToSamp,]
    x=rowsData[,1]
    y=rowsData[,2]
    spXY = SP1(rowsData)
    sqSxSy = sqrt(SS2(x)*SS2(y))
    return (spXY/sqSxSy)
  }
  sampCor12(data)

  
  #SampCor21
  sampCor21 <- function(xyFrame,rowsToSamp){
    rowsData = xyFrame[rowsToSamp,]
    x=rowsData[,1]
    y=rowsData[,2]
    spXY = SP2(rowsData)
    sqSxSy = sqrt(SS1(x)*SS1(y))
    return (spXY/sqSxSy)
  }
  sampCor21(data)
  cor(data)
  
  #SampCor22
  sampCor22 <- function(xyFrame,rowsToSamp){
    rowsData = xyFrame[rowsToSamp,]
    x=rowsData[,1]
    y=rowsData[,2]
    spXY = SP2(rowsData)
    sqSxSy = sqrt(SS2(x)*SS2(y))
    return (spXY/sqSxSy)
  }
  sampCor22(data)
  cor(data)
  
#SS1
  SS1<-function(u){
    sum((u-sum(u)/length(u))^2)
  }
  #SS2
  SS2<-function(u){
    sum(u^2)-(((sum(u))^2)/length(u))
  }
  
#SP1
  SP1<-function(d){
    sum((d[,1]-mean(d[,1]))*(d[,2]-mean(d[,2])))
  }
  #SP2
  SP2<-function(d){
    (sum(d[,1]*d[,2]))-(prod(colSums(d))/dim(d)[1])
  
  }
  SP(data)/(sqrt(SSx(data[,1])*SSx(data[,2])))
  
  SP2(data)/(sqrt(SSx(data[,1])*SSx(data[,2])))
################
  #7/26 meeting
  
  library(boot)
  library(bootstrap)
  
  sampCorVec = c(sampCor,sampCor11,sampCor12,sampCor21,sampCor22,sampCorSuper,matrixManipulation2)
  
  
  
  #data
  n=100
  x= rnorm(n)
  y0 = rnorm(n)
  rho = .5
  y=rho*x+sqrt(1-rho^2)*y0
  data = cbind(x,y)
  B = 1000
  S=100 
  
  
  timeCorMatrix = matrix(NA,nrow=S, ncol = 7)
  colnames(timeCorMatrix, do.NULL = FALSE)
  colnames(timeCorMatrix) <- c("sampCor","sampCor11","sampCor12","sampCor21","sampCor22","sampCorSuper","matrixManipulation2")
  
  #loop to test different correlation
  #functions and to record the time 
  #taken to process each
  
  for(s in 1:S){
    #s=1
    fMax= 7
    for(f in 1:fMax){
      startTime<- Sys.time()
      
      bootObject = boot(data, sampCorVec[[f]], R=B)
      
      stopTime<- Sys.time()
      time <- stopTime - startTime
      timeCorMatrix[s, f] = time
      
    }
    
    
    ciMatrix[s,]= quantile(bootObject$t,c(.025,.975))
    
    
    
  }
  
  ci95 <- function(x){
    n = length(x)
    se = sd(x)/sqrt(n)
    return (qt(.975,df=n-1)*se)
  }
  
  colSums(timeCorMatrix)
  apply(timeCorMatrix, 2, ci95)
  
  #SampCorSuper
  sampCorSuper<-function(xyFrame,rowsToSamp){
    rowsData = xyFrame[rowsToSamp,]
    x=rowsData[,1]
    y=rowsData[,2]
    
    #length(u)
    rowsLength = dim(rowsData)[1]
   
    ##sp2
    colSumsRows = colSums(rowsData)
    part1 = sum(x*y)
    part2 = prod(colSumsRows)/rowsLength
    
    spXY= part1-part2
  
    #ss2
    
    ss2x = sum(x^2)-((colSumsRows[1]^2)/rowsLength)
    ss2y = sum(y^2)-((colSumsRows[2]^2)/rowsLength)
  
    sqSxSy = sqrt(ss2x*ss2y)
    
    return (spXY/sqSxSy)
  }
  
  sampCorSuper(data)
  cor(data)


  
##########################################
  #Matrix

  xMat = matrix(c(1,2,3,4),nrow=4, ncol=1)
  yMat = matrix(c(5,4,3,2),nrow = 4,ncol=1)
  
 
  
matrixManipulation <- function(xMat, yMat){
  
  N= length(xMat)
  ones = matrix((rep(1,N)),nrow=N, ncol=1)
  xOneMat = cbind(ones,xMat)
  
  xOneMatT = t(xOneMat)
  
  inverseBrackets = inverse2x2(xOneMatT%*%xOneMat)
  
  
  
  xS = (inverseBrackets %*% xOneMatT)
  
  b1Ans = (xS%*%yMat)[2]
  return (b1Ans)
  
}

debug(matrixManipulation)
matrixManipulation(x,y)
undebug(matrixManipulation)

#function to test with
#xy then yx

tester <- function(xMat,yMat){
  byx = matrixManipulation(xMat,yMat)
  bxy = matrixManipulation(yMat, xMat)
  
  return (sqrt(byx*bxy)*sign(byx))

}
  tester(xMat,yMat)
  
  x= c(1:5)
  y= c(0,0,1,0,0)
  tester(x,y)
  cor(x,y)

# manual solve function for 2x2 matrices
inverse2x2 <- function(input){
 a = input[1,1]
 b = input[1,2]
 cc = input[2,1]
 d = input[2,2]
 
 dett= 1 / ((a*d)-(b*cc))
 invMat = matrix(c(d,-b,-cc,a),nrow=2, ncol=2, byrow = TRUE)
 finalMat = dett*invMat
 return (finalMat)
 
 }
inverse2x2(newMatT%*%newMat)
solve(newMatT%*%newMat)


debug(matrixManipulation)
matrixManipulation(xMat,yMat)

#Matrix manipulator formatted for loop

matrixManipulation2 <- function(xyFrame, rowsToSamp){
  rowsData = xyFrame[rowsToSamp,]
  x=rowsData[,1]
  y=rowsData[,2]
  
  N= length(x)
  ones = matrix((rep(1,N)),nrow=N, ncol=1)
  xOneMat = cbind(ones,x)
  
  xOneMatT = t(xOneMat)
  
  inverseBrackets = inverse2x2(xOneMatT%*%xOneMat)
  
  
  
  xS = (inverseBrackets %*% xOneMatT)
  
  b1Ans = (xS%*%y)[2]
  return (b1Ans)
  
}

