# Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes


### Data preparation work
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
# 1.	Introduction
Handwritten data recognition is in use for a long time.  Classifying handwritten digits has great importance for many real-world scenarios like recognition of zip codes on letters, numbers in hand-fill forms, amount on the check, online handwriting recognition on tablets and so on.  However, it is still a big challenge to do the classification.  Problems with handwritten digits are not always of same size, thickness, position, or orientation to the margin.  Variety and uniqueness of different handwritings of different individuals also affect appearance and formation of the digits.  And many of the current technologies are insufficient for recognition and classify them correctly due to different styles and handwritings.
This project is focus on recognizing the handwritten digits (“7” and “9”) scanned from the ZIP codes written on samples of U.S. postal envelopes.  The postal service can scan postal codes on envelopes to automate grouping of envelopes which has to be sent to the same place.  Perfect recognition of these codes is necessary in order to sort mail automatically and efficiently. 
The main purpose of this project is to represent the data in much fewer dimensions and use such representations for classification of the two digits.  We are implementing two pattern classification methods, principal component analysis (PCA) and Fisher’s linear discriminant analysis (LDA), to recognize handwritten digit.

# 2.	Dataset
In this paper we have used handwritten digit recognition our database scanned from the ZIP codes written on samples of U.S. postal envelopes with digits “7” and “9” only. Each digit image (resulting from a 16 × 16 grayscale matrix) is represented by a 256-dimensional vector of normalized intensity values.  There are two datasets, one is a training set of 800 observations (400 for each digit), and the other is a test set of 489 (245 “7” s and 244 “9” s). 

# 3.	Dimension Reduction-PCA
Mapping of high-dimensional data into low-dimensional space with retaining much of its important variables is called dimensionality reduction.  Dimension reduction reduces computational cost without losing much of visualization features or information.  Principal component analysis (PCA) is one of the methods in dimension reduction to projects higher dimension data to lower space of dimensions composed by small number of input images.

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
### 3.1 PCA for digit 7
From Figure 1, a scree plot indicates the first elbow at the second principal component and the second elbow at the fifth principal component.  Thus, we will start from first four components to capture the overall variation in number 7 recognition.  And Figure 2 shows the average image of number 7 generated from mean vector of the 256 variables.

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
![Picture1](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/Picture1.png)

The synthetic digit images are generated based on the average image.  The range of the image was determined by the eigen-image multiplied by a constant.   The purpose of generating these synthetic images is to figure out the nature of data variation that each PC direction captures.  By testing different values of the constant multiplier, we can see how the synthetic image changes along the principal direction corresponding the eigen image.  As shown in Figure 3, we use 1st eigen-image as the sample with three different ranges: [-100,100], [-1,1] and [-10,10].  From the plot, range for the constant value between -10 and 10 shows enough variances that the first PC direction captures.  Thus, [-10,10] should be an appropriate range for the constant values for all four eigen images.
```{r}
##synthetic digit images 
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-100,100,10))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }
![download](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download.png)
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-1,1,0.1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }
![download-1](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-1.png)
par(mfrow=c(3,7), omi=rep(0,4), mar=rep(0.2,4))
for (i in seq(-10,10,1))
{ digit.image(mean_7_train+i*pca_loadings_pca_7[,1]) }
![download-2](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-2.png)
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
The 1st PC explains the variance in width.  When the eigen-image takes more weights, the number 7 become wider.  The 3rd PC appears to explain the variance in height.  When the eigen-image takes more weights, the number 7 become longer.  The 2nd PC captures the curviness of the head of number 7.  As the  eigen-image takes more weights, the curviness decreases.  The 4th  PC captures the elbow angle of number 7.  As the  eigen-image takes more weights, the angle increases.

![download-3](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-3.png)
![download-4](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-4.png)
![download-5](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-5.png)
With the observed (mean-centered) component scores for each principal, which are the accurate ranges of constant c from Table 1, all the ranges are within the range of -10 and 10.  So, the range of [-10,10] is appropriate to captures the accurate ranges.  After we combine four eigen images on the constant range of [-10,10] to the average image, Figure 5 shows all features of number 7 by four PCs.

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
![download-6](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-6.png)
From Figure 6, we can identify typical and atypical ‘7’s with the first two PCs.  The typical ‘7’s are based on the average of the two PCs and atypical images are based on the minimum and maximum values of each of the two PCs as shown in Figure 7.  To explain at least 80% of the total variance, at least first 16 PCs are needed to present the 256 variables adequately.

```{r}
# first two principal components
plot(pca_scores_pca_7[,1:2],col=2,pch=16)
![download-7](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-7.png)
# Identify Typical and Atypical images 
## Typical Images

typical_7<-which(pca_scores_pca_7$Comp.1>0.5 & pca_scores_pca_7$Comp.1<1.5&
        pca_scores_pca_7$Comp.2>-0.5 & pca_scores_pca_7$Comp.2<0.5)

par(mfrow=c(2,3), omi=rep(0,4), mar=rep(0.2,4))
for (i in typical_7)
{ digit.image(data.7.train[i,])}
par(mfrow=c(1,1))
![download-8](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-8.png)

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
![download-9](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-9.png)

### 3.2 PCA for digit 9

All the principal component analysis are repeated on the training data of 400 “9” s.  From Figure 8, the scree plot indicates the first elbow at the second principal component.  However, to be consistent with the analysis of number 7, we will still use first four components to capture the overall variation in number 9 recognition.  And Figure 9 shows the average image of number 9 generated from mean vector of the 256 variables.

```{r}
# PC
pca_9 <- princomp(data.9.train)
summary(pca_9)
pca_loadings_pca_9<-summary(pca_9)$loadings
pca_scores_pca_9<-as.data.frame(pca_9$scores)
plot(pca_9,main='Scree Plot - Number 9')
```
![download-10](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-10.png)
```{r}
## avarage image of 9
mean_9_train<-colMeans(data.9.train)
digit.image(mean_9_train)
```
![download-11](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-11.png)

[-10,10] are also be chosen as the range for the constant values for all four eigen images.  As shown in Figure 10, the 1st PC explains the variance in overall size.  The 2nd PC captures the variance in width.  When the eigen-image takes more weights, the number 7 become wider.  The 3rd PC appears to explain the variance of size of head.  And the 4th  PC seems to be zooming in and out on the number.

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
![download-12](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-12.png)
![download-13](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-13.png)
![download-14](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-14.png)
![download-15](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-15.png)

With the observed (mean-centered) component scores for each principal, which are the accurate ranges of constant c from Table 2, all the ranges are within the range of -10 and 10.  So, the range of [-10,10] is appropriate to captures the accurate ranges.  After we combine four eigen images on the constant range of [-10,10] to the average image, Figure 11 shows all features of number 9 by four PCs.

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
![download-16](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-16.png)

From Figure 12, we can identify typical and atypical ‘9’s with the first two PCs.  The typical ‘9’s are based on the average of the two PCs and atypical images are based on the minimum and maximum values of each of the two PCs as shown in Figure 13.  To explain at least 80% of the total variance, at least first 18 PCs are needed to present the 256 variables adequately.

```{r}
# first two principal components
plot(pca_scores_pca_9[,1:2],col=2,pch=16)
```
![download-17](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-17.png)
```{r}
# Identify Typical and Atypical images 
## Typical Images

typical_9<-which(pca_scores_pca_9$Comp.1>-2 & pca_scores_pca_9$Comp.1<0&
        pca_scores_pca_9$Comp.2>0 & pca_scores_pca_9$Comp.2<2)

par(mfrow=c(2,3), omi=rep(0,4), mar=rep(0.2,4)) # 2x5 display
for (i in typical_9)
{ digit.image(data.9.train[i,])}
par(mfrow=c(1,1))
```
![download-18](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-18.png)
```{r}
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
![download-19](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-19.png)

### 3.3 PCA for digit 7 and 9

The similar principal component analysis is continued on the combined training data of both number 7 and 9.  And Figure 15 shows the average image of number 9 generated from mean vector of the 256 variables.

```{r}
# PC
pca_79 <- princomp(data.train)
summary(pca_79)
pca_loadings_pca_79<-summary(pca_79)$loadings
pca_scores_pca_79<-as.data.frame(pca_79$scores)
plot(pca_7,main='Scree Plot - Number 7&9')
```
![download-20](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-20.png)

```{r}
## avarage image of 7
mean_79_train<-colMeans(data.train)
digit.image(mean_79_train)
```
![download-21](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-21.png)

[-10,10] are also be chosen as the range for the constant values for all four eigen images.  As shown in Figure 16, the 1st PC explains the variance among 7 and 9 which illustrate the shape change from 9 to 7.  The 2nd PC are focus on the shape of the head for shape change from 9 to 7.  The 3rd PC and 4th  PC illustrate the variance of width and angle of head of the two numbers.

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
![download-22](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-22.png)
![download-23](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-23.png)
![download-24](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-24.png)
![download-25](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-25.png)

With the observed (mean-centered) component scores for each principal, which are the accurate ranges of constant c from Table 3, all the ranges are within the range of -10 and 10.  So, the range of [-10,10] is appropriate to captures the accurate ranges.  Figure 17 shows the mean-centered principal component scores using the first two principal components.  However, there’s a significant overlap between these two different numbers, so principal component method may not be the good choice for discriminating the two digits.
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
![download-26](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-26.png)

# 4. Classification-Fisher's LDA
Fisher’s linear discriminant can be used as a supervised learning classifier. Given labeled data, the classifier can find a set of weights to draw a decision boundary, classifying the data. Fisher’s linear discriminant attempts to find the vector that maximizes the separation between classes of the projected data. Maximizing “separation” can be ambiguous. The criteria that Fisher’s linear discriminant follows to do this is to maximize the distance of the projected means and to minimize the projected within-class variance.

```{r}
col.sd <- apply(data.train, 2, sd)  # compute columnwise standard deviation
## > sum(col.sd==0)   # 18 variables with zero SD!
# [1] 18

# > summary(col.sd)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1676  0.5481  0.4588  0.7320  0.8543 

# visualize col.sd
digit.image(col.sd)
```
![download-27](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-27.png)

```{r}
# let's use only those variables with sufficient variation, say, SD>0.2
col.ind <- (1:256)[col.sd>0.2]  # this contains the indices of variables with SD>0.2 
lda.79 <- lda(data.train[,col.ind], label.train) # lda(x, grouping)

LD1 <- rep(0,256)
LD1[col.ind] <- lda.79$scaling   # linear discriminant coefficients as a 256 dim vector
digit.image(LD1)
digit.image(abs(LD1)) # visualize the absolute values of the coefficients
```
![download-28](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-28.png)
![download-29](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-29.png)

## 4.1 Evaluating Linear Discriminant Rule for original data
Fisher’s linear discriminant analysis was performed on the combined training data, using all the variables with sufficient variation which will be set as greater than 0.2 in this work. There are, in fact, 18 pixels (or variables) with zero variation in the data, which should be excluded for discrimination.  To evaluate the linear discrimination rule, three different error rates, the apparent error rate, leave-one-out cross validation error rate, and test error rate, are calculated and shown in Table 4.
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

## 4.2 Evaluating Linear Discriminant Rule with principal components
Similar Fisher’s linear discriminant analysis was performed on the combined training data, using the first two principal components from PCA analysis.  To evaluate the linear discrimination rule, three different error rates, the apparent error rate, leave-one-out cross validation error rate, and test error rate, are also calculated and shown in Table 5.  After comparing these error rates for different classifiers, we find that all three error rates are much higher.  It is because that PCA is a method to maximize variances of linear combinations.
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
![download-30](https://github.com/yutongyang0414/Hand-written-digit-recognition-for-ZIP-codes-on-Envelopes/blob/main/Figure/download-30.png)
Additional principal components can also be considered as predictors for linear discriminant analysis. To check whether addition of subsequent principal components as predictors will improve classification accuracy, all three error rates for principal components number rnage between 2 and 50 are shown in the Figure 18.  The apparent error rate and leave-one-out CV error rate significant decreased when more PCs are included.  However, the test error rate has a sharp decreased when the amount of PC is below 5.  And it starts to gradually be increasing when the number of PCs is higher than 5.  It can be explained by the over fitting after more PCs are included in the model.

# 5.	Conclusion
The most interesting part from my analysis is the eigen-image part.  It gives me a new learning for the application of eigen-vector.  Since eigen vectors provide different weights to a variable, different PCs will explain different parts of the variance.  Although sometimes, the parts that the variances explained by the eigen vectors might not be obvious. But if the images are more complex, it will be much easier for us to find the pattern.
However, this analysis still has some limitations.  From our analysis, we conclude that PCA method cannot give us appropriate classification for these two numbers using the first two principal components.  Examining only the first few PCs might lead to significant information loss. And we only find that Linear Discriminant Analysis is a more appropriate method.  But I think if we include more PCs in the PCA method, we mays till have the chance to classify these two different numbers with PCA method.
For future work, I would also like to include all 11 numbers into the dataset.  We all know that there are some pairs of number looks similar like 3&9, 2&1. If we can do the multi-class classification, it will be more useful for the real application.

