### RANDOM FOREST red ###
# grafico curva accuracy
# plot importanza delle covariate da importance SD

setwd("/Users/FrancescoMaria/Desktop/AppliedStat/PortugheseWinesDS")
rm(list = ls())
library(randomForest)

# Random forest su buoni e cattivi senza i 6 per:

# - red raw
# - red best raw
# - red GPT
# - red best GPT

red <- read.csv("winequality-red.csv", header=T, sep=";")
rating <- (red[, 12])
red[, 6] <- (red[, 6]/red[, 7])
names(red)[6] <- 'sulfur.dioxide.ratio'
red <- red[, -7]
#red <- red[, -11]
lines <- which(rating!=6)
rating <- rating[lines]
red <- red[lines ,]
rating <- as.factor(rating)
red$quality <- ifelse(red$quality==7 | red$quality==8 | red$quality==9, 1, 0 )
head(red)

good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')
w <- red 
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

# RED RAW

w <- red
head(w)

# one pred try
n <- nrow(w)
n_train <- round(0.8 * n)
train_indices <- sample(1:n, n_train) 
train <- w[train_indices, ] 
test <- w[-train_indices, ]

set.seed(113)

model <- randomForest(quality ~ . , data = train, ntree = 200, replace=TRUE, importance=TRUE, localImp=FALSE)
rf.obj <- randomForest(quality ~ ., data=train, ntree = 200)

varImpPlot(rf.obj, col = 'brown1', cex=1.4, pch = 19, main = 'Red variable importance')
summary(model)

print(model)

pred <- predict(object = model, newdata = test, type = "class")
n_test<-n-n_train

pred<-round(pred)
pred<- as.data.frame(pred)
test <- cbind(test, pred)

accuracy <- sum(test$pred == test$quality) / length(test$quality)
accuracy

table(pred= test$pred, true=test$quality)


# Idea of plotting importances
plot(model[["importanceSD"]],type = 'h', lwd = 10)
model[["importanceSD"]]

plot(model[["importance"]])
sum(model[["importanceSD"]])

# real accuracy
len <- 100

acc <- numeric(len)

for (i in 1:len){
  set.seed(i*123)
  n <- nrow(w)
  n_train <- round(0.9 * n)
  train_indices <- sample(1:n, n_train) 
  train <- w[train_indices, ] 
  test <- w[-train_indices, ]
  
  model <- randomForest(quality ~ . , data = train, ntree = 200) #try to set different numbers
  
  pred <- predict(object = model, newdata = test, type = "class")
  n_test<-n-n_train
  pred<-pred+rnorm(n_test, mean = 0, sd = 0)
  pred<-round(pred)
  pred<- as.data.frame(pred)
  test <- cbind(test, pred)
  
  accuracy <- sum(test$pred == test$quality) / length(test$quality)
  acc[i] <- accuracy
}

mean(acc) # 0.9320312 with 0.8
          # 0.9369792 with 0.9



# red best raw

w <-data.frame( volatile.acidity	= red$volatile.acidity,
                citric.acid	= red$citric.acid,
                sulfur.dioxide.ratio	= red$sulfur.dioxide.ratio,
                sulphates = red$sulphates,
                alcohol = red$alcohol,
                quality = red$quality
)

n <- nrow(w)
n_train <- round(0.8 * n)
train_indices <- sample(1:n, n_train) 
train <- w[train_indices, ] 
test <- w[-train_indices, ]

model <- randomForest(quality ~ . , data = train, ntree = 100, replace=TRUE, importance=TRUE, localImp=FALSE)
rf.obj <- randomForest(quality ~ ., data=train, ntree = 200)
summary(model)
varImpPlot(rf.obj, col = 'red', cex=1.4, main = 'Red variable importance')

print(model)

pred <- predict(object = model, newdata = test, type = "class")
n_test<-n-n_train

pred<-round(pred)
pred<- as.data.frame(pred)
test <- cbind(test, pred)

accuracy <- sum(test$pred == test$quality) / length(test$quality)
accuracy

table(pred= test$pred, true=test$quality)

len <- 100

acc <- numeric(len)

for (i in 1:len){
  set.seed(i*123)
  n <- nrow(w)
  n_train <- round(0.95 * n)
  train_indices <- sample(1:n, n_train) 
  train <- w[train_indices, ] 
  test <- w[-train_indices, ]
  
  model <- randomForest(quality ~ . , data = train, ntree = 200) #try to set different numbers
  
  pred <- predict(object = model, newdata = test, type = "class")
  n_test<-n-n_train
  pred<-pred+rnorm(n_test, mean = 0, sd = 0)
  pred<-round(pred)
  pred<- as.data.frame(pred)
  test <- cbind(test, pred)
  
  accuracy <- sum(test$pred == test$quality) / length(test$quality)
  acc[i] <- accuracy
}

acc
mean(acc) # 0.9344271 su 100 tentativi con 0.8
          # 0.9377083 su 100 tentativi con 0.9

# red GPT

w <- data.frame(SulfurDioxide2ChlorideRatio = red$sulfur.dioxide.ratio / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,
                Chloride2Sulfate.Ratio = red$chlorides / red$sulphates,
                
                SugarAlcohol = red$residual.sugar-red$alcohol, 
                SulphurDioxideIndex = red$sulfur.dioxide.ratio+red$chlorides, 
                BalanceAroma = red$volatile.acidity-red$sulphates,
                AlcohlpH = red$alcohol/(red$pH^2),
                
                AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates, 
                AcidAlcoholInteraction = red$fixed.acidity * red$alcohol,
                quality = red$quality)

n <- nrow(w)
n_train <- round(0.8 * n)
train_indices <- sample(1:n, n_train) 
train <- w[train_indices, ] 
test <- w[-train_indices, ]

model <- randomForest(quality ~ . , data = train, ntree = 100, replace=TRUE, importance=TRUE, localImp=FALSE)

summary(model)

print(model)

pred <- predict(object = model, newdata = test, type = "class")
n_test<-n-n_train

pred<-round(pred)
pred<- as.data.frame(pred)
test <- cbind(test, pred)

accuracy <- sum(test$pred == test$quality) / length(test$quality)
accuracy

table(pred= test$pred, true=test$quality)

# real accuracy
len <- 100

acc <- numeric(len)

for (i in 1:len){
  set.seed(i*123)
  n <- nrow(w)
  n_train <- round(0.8 * n)
  train_indices <- sample(1:n, n_train) 
  train <- w[train_indices, ] 
  test <- w[-train_indices, ]
  
  model <- randomForest(quality ~ . , data = train, ntree = 200) #try to set different numbers
  
  pred <- predict(object = model, newdata = test, type = "class")
  n_test<-n-n_train
  pred<-pred+rnorm(n_test, mean = 0, sd = 0)
  pred<-round(pred)
  pred<- as.data.frame(pred)
  test <- cbind(test, pred)
  
  accuracy <- sum(test$pred == test$quality) / length(test$quality)
  acc[i] <- accuracy
}

acc
mean(acc) # 0.9314062 su 100 tentativi con 0.8
          # 0.9353125 su 100 tentativi con 0.9

# red best GPT

w <- data.frame(SulfurDioxide2ChlorideRatio = red$sulfur.dioxide.ratio / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,
                Chloride2Sulfate.Ratio = red$chlorides / red$sulphates,
                
                SugarAlcohol = red$residual.sugar-red$alcohol, 
                #SulphurDioxideIndex = red$sulfur.dioxide.ratio+red$chlorides, 
                BalanceAroma = red$volatile.acidity-red$sulphates,
                AlcohlpH = red$alcohol/(red$pH^2),
                
                AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates, 
                AcidAlcoholInteraction = red$fixed.acidity * red$alcohol,
                quality = red$quality
)

n <- nrow(w)
n_train <- round(0.8 * n)
train_indices <- sample(1:n, n_train) 
train <- w[train_indices, ] 
test <- w[-train_indices, ]

model <- randomForest(quality ~ . , data = train, ntree = 100, replace=TRUE, importance=TRUE, localImp=FALSE)

summary(model)

print(model)

pred <- predict(object = model, newdata = test, type = "class")
n_test<-n-n_train

predr<-round(pred)
pred<- as.data.frame(pred)
predr<- as.data.frame(predr)
test <- cbind(test, predr)
lines <- which(test$predr==test$quality)
sum(pred[-lines,])/length(test$quality)

accuracy <- sum(test$predr == test$quality) / length(test$quality)
accuracy

table(pred= test$pred, true=test$quality)

# real accuracy
len <- 100

acc <- numeric(len)

for (i in 1:len){
  set.seed(i*123)
  n <- nrow(w)
  n_train <- round(0.9 * n)
  train_indices <- sample(1:n, n_train) 
  train <- w[train_indices, ] 
  test <- w[-train_indices, ]
  
  model <- randomForest(quality ~ . , data = train, ntree = 200) #try to set different numbers
  
  pred <- predict(object = model, newdata = test, type = "class")
  n_test<-n-n_train
  pred<-pred+rnorm(n_test, mean = 0, sd = 0)
  pred<-round(pred)
  pred<- as.data.frame(pred)
  test <- cbind(test, pred)
  
  accuracy <- sum(test$pred == test$quality) / length(test$quality)
  acc[i] <- accuracy
}

acc
mean(acc) # 0.9304687 su 100 tentativi con 0.8
          # 0.9342708 su 100 tentativi con 0.9
