setwd("C:/Users/leoma/OneDrive/Documents/PoliMi/Applied Statistics/R/Progetto")
library(GGally)
library(rgl)

####RAW DATA - WHITE####
###Define w###
rm(list=ls())
white <- read.csv("winequality-white.csv", header=T, sep=";")
rating <- (white[, 12])
white[, 6] <- white[, 6]/white[, 7]
white <- white[, -7]
white <- white[, -11]

w <- scale(white)
pca <- princomp(w)
x11()
plot(pca$scores[, 1:2], col=ifelse(rating==6, 'red', 'black'), pch=16)
legend('topright', legend = c('6', 'Not 6'), fill = c('red', 'black'))
good1 <- ifelse(rating==6, '1', '0')

sum <- 0
for (i in 1:1000)
{
  set.seed(1326*i)
  inx <- runif(100, 0, dim(w)[1])
  fit <- manova(as.matrix(w[inx ,]) ~ good1[inx])
  s <- summary(fit)
  sum <- ifelse(s$stats[11]<0.05, sum+1, sum)
}
sum/1000

lines <- which(rating!=6)
rating <- rating[lines]
white <- white[lines ,]
rating <- as.factor(rating)

w <- data.frame(volatile = white$volatile.acidity,
                chlorides = white$chlorides, 
                sulphur.ratio = white$free.sulfur.dioxide, 
                density = white$density, alcohol = white$alcohol)
w <- data.frame(scale(w))
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')

###KNN###
#Now code to estimate k through cross validation
data <- cbind(w, good)

library(caret)
library(class)
pos_k <- seq(35, 45, by=1)
res <- matrix(rep(0, length(pos_k)*2), nrow = 2, ncol = length(pos_k))
fold <- 15
i <- 1
for (k in pos_k)
{
  error.fin <- rep(0, 20)
  for (j in 1:20)
  {
    set.seed(1836*j)
    cv_errors <- NULL
    for (inx in 1:fold)
    {
      fold_idx <- seq(from = inx, to = nrow(data), by = fold)
      val <- data[fold_idx ,]
      train <- data[-fold_idx ,]
      model <- knn(train[, -ncol(train)], val[, -ncol(val)],
                   train[, ncol(train)], k = k)
      cv_errors[inx] <- mean(model != val[, ncol(val)])
    }
    error.fin[j] <- mean(cv_errors)
  }
  AER <- mean(error.fin)
  res[1, i] <- k
  res[2, i] <- AER
  i <- i+1
}

View(res)

#See how the error is made
k <- 37
AER.tot <- 0
conf_mat1 <- matrix((rep(0,4)), 2, 2)

for (j in 1:20)
{
  set.seed(141*j)
  cv_errors <- NULL
  conf_mat <- matrix((rep(0,4)), 2, 2)
  for (inx in 1:fold)
  {
    fold_idx <- seq(from = inx, to = nrow(data), by = fold)
    val <- data[fold_idx ,]
    train <- data[-fold_idx ,]
    model <- knn(train[, -ncol(train)], val[, -ncol(val)],
                 train[, ncol(train)], k = k)
    cv_errors[inx] <- mean(model != val[, ncol(val)])
    a <- table(true=val[, ncol(val)] , predicted=model)
    conf_mat[1,1] <- conf_mat[1,1]+a[1]
    conf_mat[2,1] <- conf_mat[2,1]+a[2]
    conf_mat[1,2] <- conf_mat[1,2]+a[3]
    conf_mat[2,2] <- conf_mat[2,2]+a[4]
  }
  AER <- mean(cv_errors)
  conf_mat/fold
  AER.tot <- AER.tot + AER
  conf_mat1 <- conf_mat1+(conf_mat/fold)
}
AER.tot/20
conf_mat1/20

###LDA###
###Assunzioni###
library(mvnormtest)
library(MVN)
hist(w$volatile)
hist(w$chlorides)
hist(w$sulphur.ratio)
hist(w$density)
hist(w$alcohol)

m <- 0
p <- 0
values <- rep(0, dim(w)[2])
for (i in 1:100)
{
  set.seed(1233*i)
  inx <- sample(dim(w)[1], 50)
  test <- w[inx ,]
  p <- p+mshapiro.test(t(test))$p
  m <- m+mvn(test)$multivariateNormality$p
  for (j in 1:length(values))
  {
    if (shapiro.test(test[,j])$p > 0.01)
      values[j] <- values[j]+1
  }
}
p/100
m/100
View(rbind(names(w), values/100))

good <- ifelse(rating=='7' | rating == '8' | rating == '9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')
n0 <- length(i0)
n1 <- length(i1)
n <- n0+n1
m <- colMeans(w)
m0 <- colMeans(w[i0 ,])
m1 <- colMeans(w[i1 ,])
S0 <- cov(w[i0 ,])
S1 <- cov(w[i1 ,])
library(heplots)
summary(boxM(w, good))
x11()
par(mfrow=c(1,2))
image(S0, xlab = colnames(w))
image(S1)

library(MASS)

data <- cbind(w, good)
model <- lda((good)~ .,data=w, CV=T)
mean(model$class!=good)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- lda((good)~ .,data=data.frame(train), prior = c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val))$class)
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val))$class)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

###QDA###

data <- cbind(w, good)
model <- qda((good)~ .,data=w, CV=T)
mean(model$class!=good)
model <- qda((good)~ .,data=w)
mean(good!=predict(model, w)$class)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- qda((good)~ .,data=data.frame(train), method="t", prior = c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val))$class)
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val))$class)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

####Top index - white####
#TOP territorio: 
# Alcohol2Density.Ratio, AlcoholContent, SulfurDioxide2ChlorideRatio, 
# Chloride2Sulfate.Ratio, AcidityInteraction

#TOP qlty:
# AlcohlpH, SugarAlcohol, DensityClarity,...Fermentability

#TOP ferm:
# AlcoholSugarSquare, DensityAlcoholInteraction, ChloridePHRatio, 
# AcidAlcoholInteraction, SugarChlorideInteraction, SugarAlcoholRatio
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
w <- data.frame(AlcoholContent = white$alcohol,
                SulfurDioxide2ChlorideRatio = white$free.sulfur.dioxide / white$chlorides,
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,
                AcidityInteraction = white$fixed.acidity * white$volatile.acidity,
                 
                AlcohlpH = white$alcohol/(white$pH^2),
                SugarAlcohol = white$residual.sugar-white$alcohol,
                DensityClarity = white$density-white$chlorides,
                 
                ChloridePHRatio = white$chlorides / white$pH,
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol,
                SugarChlorideInteraction = white$residual.sugar * white$chlorides
)
w <- data.frame(scale(w))

###KNN###
#Now code to estimate k through cross validation
data <- cbind(w, good)

library(caret)
library(class)
pos_k <- seq(5, 15, by=1)
res <- matrix(rep(0, length(pos_k)*2), nrow = 2, ncol = length(pos_k))
fold <- 15
i <- 1
for (k in pos_k)
{
  error.fin <- rep(0, 20)
  for (j in 1:20)
  {
    set.seed(1836*j)
    cv_errors <- NULL
    for (inx in 1:fold)
    {
      fold_idx <- seq(from = inx, to = nrow(data), by = fold)
      val <- data[fold_idx ,]
      train <- data[-fold_idx ,]
      model <- knn(train[, -ncol(train)], val[, -ncol(val)],
                   train[, ncol(train)], k = k)
      cv_errors[inx] <- mean(model != val[, ncol(val)])
    }
    error.fin[j] <- mean(cv_errors)
  }
  AER <- mean(error.fin)
  res[1, i] <- k
  res[2, i] <- AER
  i <- i+1
}

View(res)

#See how the error is made
k <- 11
AER.tot <- 0
conf_mat1 <- matrix((rep(0,4)), 2, 2)

for (j in 1:20)
{
  set.seed(141*j)
  cv_errors <- NULL
  conf_mat <- matrix((rep(0,4)), 2, 2)
  for (inx in 1:fold)
  {
    fold_idx <- seq(from = inx, to = nrow(data), by = fold)
    val <- data[fold_idx ,]
    train <- data[-fold_idx ,]
    model <- knn(train[, -ncol(train)], val[, -ncol(val)],
                 train[, ncol(train)], k = k)
    cv_errors[inx] <- mean(model != val[, ncol(val)])
    a <- table(true=val[, ncol(val)] , predicted=model)
    conf_mat[1,1] <- conf_mat[1,1]+a[1]
    conf_mat[2,1] <- conf_mat[2,1]+a[2]
    conf_mat[1,2] <- conf_mat[1,2]+a[3]
    conf_mat[2,2] <- conf_mat[2,2]+a[4]
  }
  AER <- mean(cv_errors)
  AER
  conf_mat/fold
  AER.tot <- AER.tot + AER
  conf_mat1 <- conf_mat1+(conf_mat/fold)
}
AER.tot/20
conf_mat1/20

###LDA###
###Assunzioni###
library(mvnormtest)
variables<-variable.names(w) #same as white
x11()
par(mfrow=c(2,5))
for(i in 1:len){
  hist(w[,i],main = paste("Histogram of" , variables[i]))
}

w <- data.frame(AlcoholContent = white$alcohol,
                SulfurDioxide2ChlorideRatio = log(white$free.sulfur.dioxide / white$chlorides),
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,
                AcidityInteraction = white$fixed.acidity * white$volatile.acidity,
                
                AlcohlpH = white$alcohol/(white$pH^2),
                SugarAlcohol = white$residual.sugar-white$alcohol,
                DensityClarity = white$density-white$chlorides,
                
                ChloridePHRatio = white$chlorides / white$pH,
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol,
                SugarChlorideInteraction = log(white$residual.sugar * white$chlorides)
)
w <- data.frame(scale(w))
variables<-variable.names(w) #same as white
x11()
par(mfrow=c(2,5))
for(i in 1:len){
  hist(w[,i],main = paste("Histogram of" , variables[i]))
}

values <- rep(0, dim(w)[2])
m <- 0
p <- 0
for (i in 1:100)
{
  set.seed(1233*i)
  inx <- sample(dim(w)[1], 100)
  test <- w[inx ,]
  p <- p+mshapiro.test(t(test))$p
  m <- m+mvn(test)$multivariateNormality$p
  
  for (j in 1:length(values))
  {
    if (shapiro.test(test[,j])$p > 0.01)
      values[j] <- values[j]+1
  }
}
p/100
m/100
View(rbind(names(w), values/100))

good <- ifelse(rating=='7' | rating == '8' | rating == '9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')
n0 <- length(i0)
n1 <- length(i1)
n <- n0+n1
m <- colMeans(w)
m0 <- colMeans(w[i0 ,])
m1 <- colMeans(w[i1 ,])
S0 <- cov(w[i0 ,])
S1 <- cov(w[i1 ,])

library(heplots)
summary(boxM(w, good))
x11()
par(mfrow=c(1,2))
image(S0)
image(S1)

library(MASS)

data <- cbind(w, good)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- lda((good)~ .,data=data.frame(train), prior=c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val))$class)
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val))$class)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

###QDA###

data <- cbind(w, good)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- qda(good~ .,data=data.frame(train), method='t', prior=c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val))$class)
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val))$class)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

###Più restrittivo
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
w <- data.frame(AlcoholContent = white$alcohol,
                SulfurDioxide2ChlorideRatio = white$free.sulfur.dioxide / white$chlorides,
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,

                AlcohlpH = white$alcohol/(white$pH^2),
                DensityClarity = white$density-white$chlorides,
                
                ChloridePHRatio = white$chlorides / white$pH,
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol
)
w <- scale(w)

###KNN###
#Now code to estimate k through cross validation
data <- cbind(w, good)

library(caret)
library(class)
pos_k <- seq(10, 100, by=10)
res <- matrix(rep(0, length(pos_k)*2), nrow = 2, ncol = length(pos_k))
fold <- 15
i <- 1
for (k in pos_k)
{
  cv_errors <- NULL
  for (inx in 1:fold)
  {
    fold_idx <- seq(from = inx, to = nrow(data), by = fold)
    val <- data[fold_idx ,]
    train <- data[-fold_idx ,]
    model <- knn(train[, -ncol(train)], val[, -ncol(val)],
                 train[, ncol(train)], k = k)
    cv_errors[inx] <- mean(model != val[, ncol(val)])
  }
  AER <- mean(cv_errors)
  res[1, i] <- k
  res[2, i] <- AER
  i <- i+1
}

View(res)

#See how the error is made
k <- 30

cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- knn(train[, -ncol(train)], val[, -ncol(val)],
               train[, ncol(train)], k = k)
  cv_errors[inx] <- mean(model != val[, ncol(val)])
  a <- table(true=val[, ncol(val)] , predicted=model)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
conf_mat/fold

###LDA###
library(MASS)

data <- cbind(w, good)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- lda((good)~ .,data=data.frame(train))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val))$class)
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val))$class)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

###QDA###

data <- cbind(w, good)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- qda((good)~ .,data=data.frame(train), method='t')
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val))$class)
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val))$class)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

####TopGPT - Fisher####

good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
w <- data.frame(AlcoholContent = white$alcohol,
                SulfurDioxide2ChlorideRatio = white$free.sulfur.dioxide / white$chlorides,
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,
                AcidityInteraction = white$fixed.acidity * white$volatile.acidity,
                
                AlcohlpH = white$alcohol/(white$pH^2),
                SugarAlcohol = white$residual.sugar-white$alcohol,
                DensityClarity = white$density-white$chlorides,
                
                ChloridePHRatio = white$chlorides / white$pH,
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol,
                SugarChlorideInteraction = white$residual.sugar * white$chlorides
)
w <- scale(w)
pca <- princomp(w)
score <- pca$scores[, 1:2]

model <- lda(good ~ ., data=data.frame(w))
model$scaling
pca$loadings

###LDA - Fisher's argoument###

i0 <- which(good=='0')
i1 <- which(good=='1')
n0 <- length(i0)
n1 <- length(i1)
n <- n0+n1
m <- colMeans(w)
m0 <- colMeans(w[i0 ,])
m1 <- colMeans(w[i1 ,])
S0 <- cov(w[i0 ,])
S1 <- cov(w[i1 ,])
library(heplots)
summary(boxM(w, good))
x11()
par(mfrow=c(1,2))
image(S0)
image(S1) #Accettiamo uguaglianza covariance matrices, questo va a supporto del fatto che lda funziona meglio di qda

g <- 2 #Numero di fattori
B <- 1/g*(cbind(m0 - m) %*% rbind(m0 - m) +
            cbind(m1 - m) %*% rbind(m1 - m))
Sp  <- ((n0-1)*S0+(n1-1)*S1)/(n-g)
val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec

invSp.2 <- matrix(rep(0, length(val.Sp^2)), length(val.Sp), length(val.Sp))
for (i in 1:length(val.Sp))
{
  invSp.2 <- invSp.2 + (1/sqrt(val.Sp[i])*vec.Sp[, i]%*%t(vec.Sp[, i]))
}
invSp.2 

# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1]
a1

# second canonical coordinate
a2 <- invSp.2 %*% spec.dec$vec[,2]
a2

###Prova a proiettare su questo spazio###
spec.dec$val/sum(spec.dec$val)
score_F1 <- as.matrix(w)%*%a1
score_F2 <- as.matrix(w)%*%a2
m0 <- c(mean(score_F1[i0]), mean(score_F2[i0]))
m1 <- c(mean(score_F1[i1]), mean(score_F2[i1]))

dis <- (m0[1]+m1[1])/2
(length(which(score_F1<dis & good==0))+length(which(score_F1>dis & good==1)))/length(score_F1)

#AER
data <- cbind(w, good)
fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  i0 <- which(train[, ncol(train)] == '1')
  i1 <- which(train[, ncol(train)] == '2')
  model <- lda((good)~ .,data=data.frame(train))
  a1 <- model$scaling
  score_F1 <- as.matrix(train)[, -ncol(train)]%*%a1
  dis <- (mean(score_F1[i0])+mean(score_F1[i1]))/2
  score_F1 <- as.matrix(val)[, -ncol(val)]%*%a1
  predicted <- ifelse(score_F1<dis, '1', '2')
  cv_errors[inx] <- mean(predicted!=val[, ncol(val)])
  a <- table(true=val[, ncol(val)] , predicted=predicted)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold 

x1 <- seq(min(score_F1), max(score_F1), length=200)
x2 <- seq(min(score_F2), max(score_F2), length=200)
xy <- expand.grid(x1, x2)
z <- matrix(rep(0, dim(xy)[1]*2), dim(xy)[1], 2)
for (i in 1:dim(xy)[1])
{
  z[i, 1] <- sum((m0-xy[i ,])^2)
  z[i, 2] <- sum((m1-xy[i ,])^2)
}
z.dif <- z[, 2]-z[, 1]
x11()
plot(score_F1, score_F2, col=as.numeric(good)+5, pch=16)
contour(x1, x2, matrix(z.dif, 200), levels=0, drawlabels=F, add=T)
points(score_F1[i1], score_F2[i1], col=7, pch=16)

####Raw data-Fisher####
w <- data.frame(volatile = white$volatile.acidity,
                chlorides = white$chlorides, sulphur.ratio = white$free.sulfur.dioxide, 
                density = white$density, alcohol = white$alcohol)
w <- scale(w)
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')
pca <- princomp(w)
score <- pca$scores[, 1:2]

model <- lda(good ~ ., data=data.frame(w))
model$scaling
pca$loadings

###LDA - Fisher's argoument###

i0 <- which(good=='0')
i1 <- which(good=='1')
n0 <- length(i0)
n1 <- length(i1)
n <- n0+n1
m <- colMeans(w)
m0 <- colMeans(w[i0 ,])
m1 <- colMeans(w[i1 ,])
S0 <- cov(w[i0 ,])
S1 <- cov(w[i1 ,])
g <- 2 #Numero di fattori
B <- 1/g*(cbind(m0 - m) %*% rbind(m0 - m) +
            cbind(m1 - m) %*% rbind(m1 - m))
Sp  <- ((n0-1)*S0+(n1-1)*S1)/(n-g)
val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec

invSp.2 <- matrix(rep(0, length(val.Sp^2)), length(val.Sp), length(val.Sp))
for (i in 1:length(val.Sp))
{
  invSp.2 <- invSp.2 + (1/sqrt(val.Sp[i])*vec.Sp[, i]%*%t(vec.Sp[, i]))
}
invSp.2 

# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1]
a1

# second canonical coordinate
a2 <- invSp.2 %*% spec.dec$vec[,2]
a2

###Prova a proiettare su questo spazio###
spec.dec$val/sum(spec.dec$val)
score_F1 <- as.matrix(w)%*%a1
score_F2 <- as.matrix(w)%*%a2
m0 <- c(mean(score_F1[i0]), mean(score_F2[i0]))
m1 <- c(mean(score_F1[i1]), mean(score_F2[i1]))

dis <- (m0[1]+m1[1])/2
(length(which(score_F1<dis & good==1))+length(which(score_F1>dis & good==0)))/length(score_F1)

#AER
data <- cbind(w, good)
fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  i0 <- which(train[, ncol(train)] == '1')
  i1 <- which(train[, ncol(train)] == '2')
  model <- lda((good)~ .,data=data.frame(train))
  a1 <- model$scaling
  score_F1 <- as.matrix(train)[, -ncol(train)]%*%a1
  dis <- (mean(score_F1[i0])+mean(score_F1[i1]))/2
  score_F1 <- as.matrix(val)[, -ncol(val)]%*%a1
  predicted <- ifelse(score_F1<dis, '1', '2')
  cv_errors[inx] <- mean(predicted!=val[, ncol(val)])
  a <- table(true=val[, ncol(val)] , predicted=predicted)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold 

x1 <- seq(min(score_F1), max(score_F1), length=200)
x2 <- seq(min(score_F2), max(score_F2), length=200)
xy <- expand.grid(x1, x2)
z <- matrix(rep(0, dim(xy)[1]*2), dim(xy)[1], 2)
for (i in 1:dim(xy)[1])
{
  z[i, 1] <- sum((m0-xy[i ,])^2)
  z[i, 2] <- sum((m1-xy[i ,])^2)
}
z.dif <- z[, 2]-z[, 1]

x11()
plot(score_F1, score_F2, col=as.numeric(good)+5, pch=16)
legend('bottomleft', legend=c('bad', 'good'), fill=c(6,7))
contour(x1, x2, matrix(z.dif, 200), levels=0, drawlabels=F, add=T)
points(score_F1[i1], score_F2[i1], col=7, pch=16)

####Svm####
library(e1071)

###Raw variables###
w <- data.frame(volatile = white$volatile.acidity,
                chlorides = white$chlorides, 
                sulphur.ratio = white$free.sulfur.dioxide, 
                density = white$density, alcohol = white$alcohol)
w <- data.frame(scale(w))
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')
data <- cbind(w, good)

tune.out <- tune(svm, good~ ., data = data.frame(data), kernel = 'radial', ranges = list(cost=c(0.001,0.01,0.1,1,10), gamma=c(1,5,10,15)))

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- svm((good)~ .,data=data.frame(train), kernel = 'radial', gamma = 5, cost = 1, prior=c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val)))
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val)))
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold


w <- data.frame(AlcoholContent = white$alcohol,
                SulfurDioxide2ChlorideRatio = white$free.sulfur.dioxide / white$chlorides,
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,
                AcidityInteraction = white$fixed.acidity * white$volatile.acidity,
                
                AlcohlpH = white$alcohol/(white$pH^2),
                SugarAlcohol = white$residual.sugar-white$alcohol,
                DensityClarity = white$density-white$chlorides,
                
                ChloridePHRatio = white$chlorides / white$pH,
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol,
                SugarChlorideInteraction = white$residual.sugar * white$chlorides)

#radial/linear on chatgpt indexes
data <- cbind(w, good)

tune.out <- tune(svm, good~ ., data = data.frame(data), kernel = 'radial', ranges = list(cost=c(0.001,0.01,0.1,1,10), gamma=c(1,5,10,15)))

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- svm((good)~ .,data=data.frame(train), kernel = 'radial', gamma = 1, cost = 10, prior=c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predict(model, newdata=as.data.frame(val)))
  a <- table(true=val[, ncol(val)] , predicted=predict(model, newdata=as.data.frame(val)))
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

####Logistic regression####
library(stats)

###Our variables###
w <- data.frame(volatile = white$volatile.acidity,
                chlorides = white$chlorides, 
                sulphur.ratio = white$free.sulfur.dioxide, 
                density = white$density, alcohol = white$alcohol)
w <- data.frame(scale(w))
data <- cbind(w, good)

modello <- glm(good ~ ., data=data, family = 'binomial')
summary(modello)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- glm(good ~ .,data=train, family='binomial')
  v <- predict(model, newdata=as.data.frame(val))
  prob <- exp(v)/(1+exp(v))
  predictions <- ifelse (prob<1/3, '0', '1')
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predictions)
  a <- table(true=val[, ncol(val)] , predicted=predictions)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

###Top GPT###
w <- data.frame(AlcoholContent = white$alcohol,
                SulfurDioxide2ChlorideRatio = white$free.sulfur.dioxide / white$chlorides,
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,
                AcidityInteraction = white$fixed.acidity * white$volatile.acidity,
                
                AlcohlpH = white$alcohol/(white$pH^2),
                SugarAlcohol = white$residual.sugar-white$alcohol,
                DensityClarity = white$density-white$chlorides,
                
                ChloridePHRatio = white$chlorides / white$pH,
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol,
                SugarChlorideInteraction = white$residual.sugar * white$chlorides
)

w <- data.frame(scale(w))
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
data <- cbind(w, good)

modello <- glm(good ~ ., data=data, family = 'binomial')
summary(modello)

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- glm(good ~ .,data=train, family='binomial')
  v <- predict(model, newdata=as.data.frame(val))
  prob <- exp(v)/(1+exp(v))
  predictions <- ifelse (prob<1/3, '0', '1')
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           predictions)
  a <- table(true=val[, ncol(val)] , predicted=predictions)
  conf_mat[1,1] <- conf_mat[1,1]+a[1]
  conf_mat[2,1] <- conf_mat[2,1]+a[2]
  conf_mat[1,2] <- conf_mat[1,2]+a[3]
  conf_mat[2,2] <- conf_mat[2,2]+a[4]
}
AER <- mean(cv_errors)
AER
row.names(conf_mat) <- c('predicted-bad', 'predicted-good')
colnames(conf_mat) <- c('true-bad', 'true-good')
conf_mat/fold

