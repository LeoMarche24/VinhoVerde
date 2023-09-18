setwd("C:/Users/leoma/OneDrive/Documents/PoliMi/Applied Statistics/R/Progetto")
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
####Prova a costruire nuovi indici per avere più variabili####
####ChatGPT - Qualità delle Fermentazione####

w <- data.frame(TotalAcidity=red$fixed.acidity+red$volatile.acidity+red$citric.acid, 
                SugarAlcohol = red$residual.sugar-red$alcohol, 
                SulphurDioxideIndex = red$free.sulfur.dioxide+red$chlorides, 
                pHSulphates = red$pH-red$sulphates, 
                DensityClarity = red$density-red$chlorides, 
                Fermentability = (red$residual.sugar-red$volatile.acidity)/red$alcohol, 
                OxidationPotential = red$volatile.acidity*red$free.sulfur.dioxide,
                FlavorIntensity = red$volatile.acidity+red$citric.acid+red$sulphates, 
                BalanceAroma = red$volatile.acidity-red$sulphates, 
                SugarAcid = red$residual.sugar/red$fixed.acidity,
               # AcidityBalance =  (red$fixed.acidity^2)/red$citric.acid, 
                SweetnessBody = sqrt(red$residual.sugar)*(red$density^2),
                SulphurInteraction = (red$free.sulfur.dioxide*red$chlorides)+(red$free.sulfur.dioxide^2),
                AlcohlpH = red$alcohol/(red$pH^2),
                Complexity = exp(red$free.sulfur.dioxide)*log(1+red$volatile.acidity))


boxplot(w)

try <- scale(w)
pca <- princomp(try)
summary(pca)
pca$loadings
plot(pca$scores[, 1:2], col=rating, pch=16)

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

####ChatGPT - Fermentazione####
w<-NULL
w <- data.frame(AcidRatio = (red$fixed.acidity)/(red$volatile.acidity),
                SugarAlcoholRatio = red$residual.sugar / red$alcohol, 
                SulphurHeatRatio = red$free.sulfur.dioxide / red$alcohol, 
                ChloridePHRatio = red$chlorides / red$pH, 
                CitricAcidRatio = red$citric.acid / (red$fixed.acidity + red$volatile.acidity), 
                AcidInteraction = red$fixed.acidity * red$volatile.acidity, 
                AlcoholSugarSquare = (red$alcohol)^2,
                pHSulphateInteraction = red$pH * red$sulphates, 
                AcidAlcoholInteraction = red$fixed.acidity * red$alcohol, 
                SugarChlorideInteraction = red$residual.sugar * red$chlorides,
                AcidRatioSquare = (red$fixed.acidity / red$volatile.acidity)^2,
                DensityAlcoholInteraction = red$density * red$alcohol,
                SulphurSugarInteraction = red$free.sulfur.dioxide * red$residual.sugar,
                AcidPHInteraction = red$fixed.acidity * red$pH)
try <- scale(w)
pca <- princomp(try)
summary(pca)
pca$loadings
x11()
plot(pca$scores[, 1:2], col=as.numeric(rating), pch=16)
legend('bottomright', legend = levels(rating), fill=as.numeric(levels(rating))-2)

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

####ChatGPT - Territorio####

Acidity.Index = (red$fixed.acidity + red$volatile.acidity) - red$citric.acid
w <- data.frame(Acidity.Index = Acidity.Index,
                Sugar2Acid.Ratio = red$residual.sugar / Acidity.Index,
                SulfurDioxide2ChlorideRatio = red$free.sulfur.dioxide / red$chlorides,
                Alcohol2Density.Ratio = red$alcohol/red$density,
                Sulphates2pH.Ratio = red$sulphates/red$pH,
                TotalAcidity = red$fixed.acidity + red$volatile.acidity + red$citric.acid,
                Chloride2Sulfate.Ratio = red$chlorides / red$sulphates,
                AlcoholContent = red$alcohol,
                SugarContent = red$residual.sugar,
                pHLevel = red$pH,
                AcidityInteraction = red$fixed.acidity * red$volatile.acidity,
                SugarSquared = red$residual.sugar^2,
                ChlorideCubed = (red$free.sulfur.dioxide^2)/red$sulphates,
                pHCubed = red$pH^3)

try <- scale(w)
pca <- princomp(try)
summary(pca)
pca$loadings
plot(pca$scores[, 1:2], col=rating, pch=16)

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

####Top GPT - Red####
w <- data.frame (SulfurDioxide2ChlorideRatio = red$free.sulfur.dioxide / red$chlorides,
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
                 AcidAlcoholInteraction = red$fixed.acidity * red$alcohol)
names(w)

####Raw variables####
w <- red

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
