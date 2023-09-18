####RAW DATA - RED####
###Define w###
rm(list=ls())
red <- read.csv("winequality-red.csv", header=T, sep=";")
rating <- (red[, 12])
red[, 6] <- red[, 6]/red[, 7]
red <- red[, -7]
red <- red[, -11]
lines <- which(rating!=6)

rating <- rating[lines]
red <- red[lines ,]
rating <- as.factor(rating)

w <- data.frame(fixed = red$fixed.acidity,
                volatile.acidity = red$volatile.acidity, 
                citric.acid = red$citric.acid, 
                chlorides = red$chlorides,
                sulphur.ratio = red$free.sulfur.dioxide, 
                density = red$density,
                sulphites = red$sulphates, 
                alcohol = red$alcohol)
w <- scale(w)
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)

###KNN###
#Now code to estimate k through cross validation
data <- cbind(w, good)

library(caret)
library(class)
pos_k <- seq(45, 55, by=1)
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
k <- 50
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
library(MVN)

m <- 0
p <- 0
values <- rep(0, dim(w)[2])
for (i in 1:100)
{
  set.seed(1233*i)
  inx <- sample(dim(w)[1], 100)
  test <- w[inx ,]
  p <- p+mshapiro.test(t(test))$p
  m <- m+mvn(test)$multivariateNormality$p
  for (j in 1:length(values))
  {
    if (shapiro.test(test[,j])$p > 0.0001)
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
  model <- lda((good)~ .,data=data.frame(train),prior = c(1/3,2/3))
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
  model <- qda((good)~ .,data=data.frame(train), method='t', prior = c(1/3, 2/3))
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

####Top index - red####
#TOP territorio: 
# Alcohol2Density.Ratio, AlcoholContent, Chloride2Sulfate.Ratio,
# AcidityInteraction, SulfurDioxide2ChlorideRatio, Sulphates2pH.Ratio

#TOP qlty:
# BalanceAroma, AlcohlpH, SugarAlcohol, Complexity, pHSulphates, SulphurInteraction 

#TOP ferm:
# AlcoholSugarSquare, DensityAlcoholInteraction, AcidRatio, AcidAlcoholInteraction, 
# AcidRatioSquare, AcidInteraction, pHSulphateInteraction, CitricAcidRatio

good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)

red <- read.csv("winequality-red.csv", header=T, sep=";")
rating <- (red[, 12])
red[, 6] <- (red[, 6]/red[, 7])
names(red)[6] <- 'sulfur.dioxide.ratio'
red <- red[, -7]
red <- red[, -11]
lines <- which(rating!=6)
rating <- rating[lines]
red <- red[lines ,]
rating <- as.factor(rating)

w <- data.frame(SulfurDioxide2ChlorideRatio = red$sulfur.dioxide.ratio / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,

                SugarAlcohol = red$residual.sugar-red$alcohol, 
                SulphurDioxideIndex = red$sulfur.dioxide.ratio+red$chlorides, 

                AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates 
)
w <- scale(w)

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
k <- 10
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
library(MVN)

m <- 0
p <- 0
values <- rep(0, dim(w)[2])
for (i in 1:100)
{
  set.seed(1233*i)
  inx <- sample(dim(w)[1], 100)
  test <- w[inx ,]
  p <- p+mshapiro.test(t(test))$p
  m <- m+mvn(test)$multivariateNormality$p
  for (j in 1:length(values))
  {
    if (shapiro.test(test[,j])$p > 0.0001)
      values[j] <- values[j]+1
  }
}
p/100
m/100
View(rbind(names(w), values/100))
len <- dim(w)[2]
variables<-variable.names(w)
x11()
par(mfrow=c(2,5))
for(i in 1:len){
  hist(w[,i],main = paste("Histogram of" , variables[i]))
}

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

fold <- 15
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- qda((good)~ .,data=data.frame(train), prior = c(1/3, 2/3))
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

###Indici inventati###
yy <- dnorm(red$volatile.acidity, 0.4, 0.2)
yy1 <- dnorm(red$free.sulfur.dioxide, 0.4, 0.2)
w <- data.frame(TotalAcidityMod=(red$fixed.acidity*red$volatile.acidity)+red$citric.acid, 
                VolatileRatioMod = red$volatile.acidity/((red$fixed.acidity*red$volatile.acidity)+red$citric.acid), 
                Fundamental1 = red$residual.sugar/((red$fixed.acidity*red$volatile.acidity)+red$citric.acid),
                Fundamental2 = red$residual.sugar/red$volatile.acidity,
                Temperature = red$residual.sugar/red$fixed.acidity, 
                ChloridesInf = red$chlorides*(red$volatile.acidity/red$fixed.acidity), 
                GoodnessFermentation = yy*red$volatile.acidity*yy1*(red$free.sulfur.dioxide)*red$sulphates*red$alcohol/(red$chlorides*red$residual.sugar*red$density))
###Bootstrap anova###
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')

values <- rep(0, dim(w)[2])
for (i in 1:1000)
{
  set.seed(21534*i)
  tr0 <- runif(50, 0, length(i0))
  tr1 <- runif(50, 0, length(i1))
  train_lines <- c(i0[tr0], i1[tr1])
  
  for (j in 1:length(values))
  {
    sum <- summary(aov(w[train_lines, j] ~ good[train_lines]))
    if (sum[[1]][["Pr(>F)"]][1]<0.01)
      values[j] <- values[j]+1
  }
}
View(rbind(names(w), values/1000))

w <- data.frame(TotalAcidityMod=(red$fixed.acidity*red$volatile.acidity)+red$citric.acid, 
                ChloridesInf = red$chlorides*(red$volatile.acidity/red$fixed.acidity), 
                GoodnessFermentation = yy*red$volatile.acidity*yy1*(red$free.sulfur.dioxide)*red$sulphates*red$alcohol/(red$chlorides*red$residual.sugar*red$density))
w <- scale(w)
###KNN###
#Now code to estimate k through cross validation
data <- cbind(w, good)

library(caret)
library(class)
pos_k <- seq(10, 100, by=10)
res <- matrix(rep(0, length(pos_k)*2), nrow = 2, ncol = length(pos_k))
fold <- 5
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
k <- 40

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

fold <- 5
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- lda((good)~ .,data=data.frame(train), prior = c(0.5, 0.5))
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

fold <- 5
cv_errors <- NULL
conf_mat <- matrix((rep(0,4)), 2, 2)
for (inx in 1:fold)
{
  fold_idx <- seq(from = inx, to = nrow(data), by = fold)
  val <- data[fold_idx ,]
  train <- data[-fold_idx ,]
  model <- qda((good)~ .,data=data.frame(train))
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
conf_mat/fold #SOLO BAD.

####GPT - FIsher red####

w <- data.frame(SulfurDioxide2ChlorideRatio = red$free.sulfur.dioxide / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,
                #Chloride2Sulfate.Ratio = red$chlorides / red$sulphates,
                
                SugarAlcohol = red$residual.sugar-red$alcohol, 
                SulphurDioxideIndex = red$free.sulfur.dioxide+red$chlorides, 
                #BalanceAroma = red$volatile.acidity-red$sulphates,
                #AlcohlpH = red$alcohol/(red$pH^2),
                
                AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates 
                #AcidAlcoholInteraction = red$fixed.acidity * red$alcohol 
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
contour(x1, x2, matrix(z.dif, 200), levels=0, drawlabels=F, add=T)
points(score_F1[i1], score_F2[i1], col=7, pch=16)

#Plot del france
plot(score_F1, score_F2, col=as.numeric(good)+5, pch=16)
points(score_F1[i1], score_F2[i1], col=7, pch=16)

####Raw data-Fisher - red####
w <- data.frame(fixed = red$fixed.acidity,
                volatile.acidity = red$volatile.acidity, 
                citric.acid = red$citric.acid, 
                chlorides = red$chlorides,
                sulphur.ratio = red$free.sulfur.dioxide, 
                density = red$density,
                sulphites = red$sulphates, 
                alcohol = red$alcohol)
w <- scale(w)
pca <- princomp(w)
score <- pca$scores[, 1:2]
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)

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
legend('bottomleft', legend=c('bad', 'good'), fill=c(6,7))
contour(x1, x2, matrix(z.dif, 200), levels=0, drawlabels=F, add=T)
points(score_F1[i1], score_F2[i1], col=7, pch=16)

####Svm####
library(e1071)

###Raw variables###
w <- data.frame(fixed = red$fixed.acidity,
                volatile.acidity = red$volatile.acidity, 
                citric.acid = red$citric.acid, 
                chlorides = red$chlorides,
                sulphur.ratio = red$free.sulfur.dioxide, 
                density = red$density,
                sulphites = red$sulphates, 
                alcohol = red$alcohol)
w <- scale(w)
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
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
                           round(predict(model, newdata=as.data.frame(val))))
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


w <- data.frame(SulfurDioxide2ChlorideRatio = red$free.sulfur.dioxide / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,
                #Chloride2Sulfate.Ratio = red$chlorides / red$sulphates,
                
                SugarAlcohol = red$residual.sugar-red$alcohol, 
                SulphurDioxideIndex = red$free.sulfur.dioxide+red$chlorides, 
                #BalanceAroma = red$volatile.acidity-red$sulphates,
                #AlcohlpH = red$alcohol/(red$pH^2),
                
                AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates 
                #AcidAlcoholInteraction = red$fixed.acidity * red$alcohol 
)
w <- scale(w)

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
  model <- svm((good)~ .,data=data.frame(train), kernel = 'radial', gamma = 1, cost = 1, prior=c(1/3, 2/3))
  cv_errors[inx] <- mean((val[, ncol(val)]) != 
                           round(predict(model, newdata=as.data.frame(val))))
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
w <- data.frame(fixed = red$fixed.acidity,
                volatile.acidity = red$volatile.acidity, 
                citric.acid = red$citric.acid, 
                chlorides = red$chlorides,
                sulphur.ratio = red$free.sulfur.dioxide, 
                density = red$density,
                sulphites = red$sulphates, 
                alcohol = red$alcohol)
w <- scale(w)
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)

data <- data.frame(w, good)
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
  predictions <- ifelse (v<1/3, '0', '1')
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
w <- data.frame(SulfurDioxide2ChlorideRatio = red$free.sulfur.dioxide / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,
                #Chloride2Sulfate.Ratio = red$chlorides / red$sulphates,
                
                SugarAlcohol = red$residual.sugar-red$alcohol, 
                SulphurDioxideIndex = red$free.sulfur.dioxide+red$chlorides, 
                #BalanceAroma = red$volatile.acidity-red$sulphates,
                #AlcohlpH = red$alcohol/(red$pH^2),
                
                AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates 
                #AcidAlcoholInteraction = red$fixed.acidity * red$alcohol 
)
w <- data.frame(scale(w))
data <- data.frame(w, good)

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
  predictions <- ifelse (v<1/3, '0', '1')
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
