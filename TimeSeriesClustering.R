# Time Series Clustering

sc <- read.csv("/Users/dervisgoksun/Desktop/ClusteringTimeSeriesSelecteddata.csv", header=FALSE)

#Group 100 users, to make it easy for plotting
n <- 10
s<- sample(1:100, n)
idx <- c(s, 10+s, 20+s, 30+s, 40+s, 50+s, 60+s, 70+s, 80+s, 90+s)
sample2 <- sc[idx,]
observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n), rep(7,n), rep(8,n), rep(9,n),rep (10,n))

# compute DTW distances
#install.packages('dtw')
#install.packages('proxy')
library(dtw)
distMatrix <- dist(sc, method='DTW')

# hierarchical clustering
hc <- hclust(distMatrix, method='average')  
plot(hc, labels=observedLabels, main='') 


# Time Series Classification

# extracting DWT coefficients (with Haar filter)
#install.packages('wavelets')
library(wavelets)
wtData <- NULL
for (i in 1:nrow(sc)) {
  a <- t(sc[i,])
  wt <- dwt(a, filter='haar', boundary='periodic')
  wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
}
wtData <- as.data.frame(wtData)

# set class labels into categorical values
classId <- c(rep('1',1), rep('2',1), rep('3',1),rep('4',1), rep('5',1), rep('6',1), rep('7',1))
wtSc <- data.frame(cbind(classId, wtData))

# build a decision tree with ctree() in package party
#install.packages('party')
library(party)
ct <- ctree(classId ~ ., data=wtSc, controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
pClassId <- predict(ct)

# check predicted classes against original class labels
table(classId, pClassId)

# accuracy
(sum(classId==pClassId)) / nrow(wtSc)

plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))
