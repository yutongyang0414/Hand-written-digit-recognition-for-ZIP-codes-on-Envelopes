# Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes
# Data preparation work
```{r}
library(MASS)   # load library MASS
library(tidyverse)
# read data files --------------------------------

data.7 <- read.csv("train.7.txt", header = F)
data.7 <- as.matrix(data.7)
data.9 <- read.csv("train.9.txt", header = F)
data.9 <- as.matrix(data.9)

dim(data.7)
dim(data.9)
# > dim(data.7) # data set dimension
# [1] 645 256
# > dim(data.9)
# [1] 644 256
```

```{r include=FALSE}
i <- 1 
# turn a 256 row vector into a 16x16 matrix 
img <- matrix(as.vector(data.7[i,]), nrow = 16, ncol=16)

# display the matrix in gray scale
image(img[,16:1], col=gray((32:0)/32), axes=F)
digit.image <- function(x)
               { img <- matrix(as.vector(x), nrow = 16, ncol=16) 
               	 image(img[,16:1], col=gray((32:0)/32), axes=F)
               	 box(col=3)
}
# try the new function!
digit.image(data.7[i,])

# visualize the first five "7" and the first five "9" images
par(mfrow=c(2,5), omi=rep(0,4), mar=rep(0.2,4)) # 2x5 display
for (i in 1:5)
{ digit.image(data.7[i,]) }
for (i in 1:5)
{ digit.image(data.9[i,]) }

par(mfrow=c(1,1))
data.7.train <- data.7[1:400,]
data.7.test <- data.7[401:645,]

data.9.train <- data.9[1:400,]
data.9.test <- data.9[401:644,]

data.train <- rbind(data.7.train, data.9.train)

label.train <- c(rep(7,400), rep(9,400))

data.test <- rbind(data.7.test, data.9.test)
label.test <- c(rep(7,245), rep(9,244))

```

# PCA
### PCA for digit 7

```{r}
# PC
pca_7 <- princomp(data.7.train)
summary(pca_7)
pca_loadings_pca_7<-summary(pca_7)$loadings
pca_scores_pca_7<-as.data.frame(pca_7$scores)
plot(pca_7,main='Scree Plot - Number 7')
```
```{r}
## avarage image of 7
mean_7_train<-colMeans(data.7.train)
digit.image(mean_7_train)
```

```{r}
##synthetic digit images 
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-100,100,10))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-1,1,0.1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }





# Visualizing average image + (constant)*eigen_image
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,2]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,3]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,4]) }
par(mfrow=c(1,1))
```

```{r}
## Range of observed component scores 
range_7_min = rep(0,4)
range_7_max = rep(0,4)

for(i in 1:4){
  range_7_min[i] = min(pca_scores_pca_7[,i])
  range_7_max[i] = max(pca_scores_pca_7[,i])
}

range_7 = as.data.frame( cbind(c(1,2,3,4),range_7_min,range_7_max))
names(range_7) = c('PC','Min','Max')

range_7

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4)) # 2x5 display
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]+
                i*pca_loadings_pca_7[,2]+
                i*pca_loadings_pca_7[,3]+
                i*pca_loadings_pca_7[,4]) }
par(mfrow=c(1,1))
```

```{r}
# first two principal components
plot(pca_scores_pca_7[,1:2],col=2,pch=16)

# Identify Typical and Atypical images 
## Typical Images

typical_7<-which(pca_scores_pca_7$Comp.1>0.5 & pca_scores_pca_7$Comp.1<1.5&
        pca_scores_pca_7$Comp.2>-0.5 & pca_scores_pca_7$Comp.2<0.5)

par(mfrow=c(2,3), omi=rep(0,4), mar=rep(0.2,4))
for (i in typical_7)
{ digit.image(data.7.train[i,])}
par(mfrow=c(1,1))


## Atypical Images

which.max(pca_scores_pca_7[,1])
which.min(pca_scores_pca_7[,1])
which.max(pca_scores_pca_7[,2])
which.min(pca_scores_pca_7[,2])

par(mfrow=c(2,2), omi=rep(0,4), mar=rep(0.2,4))
for (i in c(21,282,378,37))
{ digit.image(data.7.train[i,])}
par(mfrow=c(1,1))
```

### PCA for digit 9

```{r}
# PC
pca_9 <- princomp(data.9.train)
summary(pca_9)
pca_loadings_pca_9<-summary(pca_9)$loadings
pca_scores_pca_9<-as.data.frame(pca_9$scores)
plot(pca_9,main='Scree Plot - Number 9')
```
```{r}
## avarage image of 9
mean_9_train<-colMeans(data.9.train)
digit.image(mean_9_train)
```

```{r}
# Visualizing average image + (constant)*eigen_image
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_9[,1]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_9[,2]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_9[,3]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_9[,4]) }
par(mfrow=c(1,1))
```

```{r}
## Range of observed component scores 
range_9_min = rep(0,4)
range_9_max = rep(0,4)

for(i in 1:4){
  range_9_min[i] = min(pca_scores_pca_9[,i])
  range_9_max[i] = max(pca_scores_pca_9[,i])
}

range_9 = as.data.frame( cbind(c(1,2,3,4),range_9_min,range_9_max))
names(range_9) = c('PC','Min','Max')

range_9

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_9_train+i*pca_loadings_pca_9[,1]+
                i*pca_loadings_pca_9[,2]+
                i*pca_loadings_pca_9[,3]+
                i*pca_loadings_pca_9[,4]) }
par(mfrow=c(1,1))
```

```{r}
# first two principal components
plot(pca_scores_pca_9[,1:2],col=2,pch=16)

# Identify Typical and Atypical images 
## Typical Images

typical_9<-which(pca_scores_pca_9$Comp.1>-2 & pca_scores_pca_9$Comp.1<0&
        pca_scores_pca_9$Comp.2>0 & pca_scores_pca_9$Comp.2<2)

par(mfrow=c(2,3), omi=rep(0,4), mar=rep(0.2,4)) # 2x5 display
for (i in typical_9)
{ digit.image(data.9.train[i,])}
par(mfrow=c(1,1))


## Atypical Images

which.max(pca_scores_pca_9[,1])
which.min(pca_scores_pca_9[,1])
which.max(pca_scores_pca_9[,2])
which.min(pca_scores_pca_9[,2])

par(mfrow=c(2,2), omi=rep(0,4), mar=rep(0.2,4))
for (i in c(39,156,7,163))
{ digit.image(data.9.train[i,])}
par(mfrow=c(1,1))
```
### PCA for digit 7 and 9

```{r}
# PC
pca_79 <- princomp(data.train)
summary(pca_79)
pca_loadings_pca_79<-summary(pca_79)$loadings
pca_scores_pca_79<-as.data.frame(pca_79$scores)
plot(pca_7,main='Scree Plot - Number 7&9')
```
```{r}
## avarage image of 7
mean_79_train<-colMeans(data.train)
digit.image(mean_79_train)
```

```{r}
# Visualizing average image + (constant)*eigen_image
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_79_train+i*pca_loadings_pca_79[,1]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_79_train+i*pca_loadings_pca_79[,2]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_79_train+i*pca_loadings_pca_79[,3]) }
par(mfrow=c(1,1))

par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_79_train+i*pca_loadings_pca_79[,4]) }
par(mfrow=c(1,1))
```

```{r}
## Range of observed component scores 
range_79_min = rep(0,4)
range_79_max = rep(0,4)

for(i in 1:4){
  range_79_min[i] = min(pca_scores_pca_79[,i])
  range_79_max[i] = max(pca_scores_pca_79[,i])
}

range_79 = as.data.frame( cbind(c(1,2,3,4),range_79_min,range_79_max))
names(range_79) = c('PC','Min','Max')

range_79

```

```{r}

# Plot of Mean-Centered PC Scores
number<-as.factor(label.train)
pc_score_79 <-as.data.frame(pca_79$scores[,1:2])
pc_score_79['label']<-label.train
ggplot(data= as.data.frame(pca_79$scores[,1:2]),aes(x=Comp.1,y=Comp.2,col=number))+
  geom_point()
```

#LDA
```{r}
col.sd <- apply(data.train, 2, sd)  # compute columnwise standard deviation
## > sum(col.sd==0)   # 18 variables with zero SD!
# [1] 18

# > summary(col.sd)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1676  0.5481  0.4588  0.7320  0.8543 

# visualize col.sd
digit.image(col.sd)

# let's use only those variables with sufficient variation, say, SD>0.2
col.ind <- (1:256)[col.sd>0.2]  # this contains the indices of variables with SD>0.2 
lda.79 <- lda(data.train[,col.ind], label.train) # lda(x, grouping)

LD1 <- rep(0,256)
LD1[col.ind] <- lda.79$scaling   # linear discriminant coefficients as a 256 dim vector
digit.image(LD1)
digit.image(abs(LD1)) # visualize the absolute values of the coefficients
```

## Evaluating Linear Discriminant Rule for original data

```{r}
# Evaluating Linear Discriminant Rule
## Apparent Error Rate
pred.tr = predict(lda.79)
original_apparent_error = mean(label.train!=pred.tr$class)
original_apparent_error 


## Leave-one-out
lda.79.cv <- lda(data.train[,col.ind], label.train,CV=T)
original_leave_one_out_error = mean(label.train!=lda.79.cv$class)
original_leave_one_out_error

## Test Error Rate
pred.test = predict(lda.79,newdata=data.test[,col.ind])
original_test_error = mean(label.test!=pred.test$class)
original_test_error

rbind(original_apparent_error,original_leave_one_out_error,original_test_error)
```

## Evaluating Linear Discriminant Rule with principal components
```{r}
lda.79.pc <- lda(label~.,data=pc_score_79)

## Apparent Error Rate
pred.tr.pc = predict(lda.79.pc)
two_pcs_apparent_error = mean(label.train!=pred.tr.pc$class)
two_pcs_apparent_error


## Leave-one-out
lda.79.cv.pc <- lda(label~.,data=pc_score_79,CV=T)
two_pcs_leave_one_out_error= mean(label.train!=lda.79.cv.pc$class)
two_pcs_leave_one_out_error

## Test Error Rate
data.test.pc.scores = as.data.frame(princomp(data.test)$scores[,1:2])
pred.test.pc = predict(lda.79.pc,newdata=data.test.pc.scores)
two_pcs_test_error= mean(label.test!=pred.test.pc$class)
two_pcs_test_error

rbind(two_pcs_apparent_error,two_pcs_leave_one_out_error,two_pcs_test_error)
```


```{r LDA b-additional PCs}
#LDA using PCs

apparent_error_rate = rep(0,50)
leave_one_out_error_rate = rep(0,50)
test_error_rate = rep(0,50)

for (i in 2:51){
  
  pc_score_79_ipcs = as.data.frame(pca_79$scores[,1:i])
  pc_score_79_ipcs['label']=label.train
  
  lda.79.ipcs <- lda(label~.,data=pc_score_79_ipcs)
  
  ## Apparent Error Rate
  pred.tr.ipcs = predict(lda.79.ipcs)
  apparent_error_rate[i-1]=mean(label.train!=pred.tr.ipcs$class)

    ## Leave-one-out
  lda.79.cv.ipcs = lda(label~.,data=pc_score_79_ipcs,CV=T)
  leave_one_out_error_rate[i-1] = mean(label.train!=lda.79.cv.ipcs$class)
  
  ## Test Error Rate
  data.test.pc.scores = as.data.frame(princomp(data.test)$scores[,1:i])
  pred.test.pc = predict(lda.79.ipcs,newdata=data.test.pc.scores)
  test_error_rate[i-1]= mean(label.test!=pred.test.pc$class)

  }

result_comparison = cbind(apparent_error_rate,leave_one_out_error_rate,test_error_rate)
result_comparison = as.data.frame(result_comparison)
result_comparison['Number of PCs']=c(2:51)
result_comparison%>%
  gather(key='error rate type',value='error rate', apparent_error_rate:test_error_rate)%>%
  ggplot(aes(x=`Number of PCs`,y=`error rate`,col=`error rate type`))+
  geom_line()
```




