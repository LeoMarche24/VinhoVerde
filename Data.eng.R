setwd("C:/Users/leoma/OneDrive/Documents/PoliMi/Applied Statistics/R/Progetto")
rm(list = ls())

white <- read.csv("winequality-white.csv", header=T, sep=";")
rating <- (white[, 12])
white[, 6] <- white[, 6]/white[, 7]
white <- white[, -7]
white <- white[, -11]
lines <- which(rating!=6)

rating <- rating[lines]
white <- white[lines ,]
rating <- as.factor(rating)

####Prova a costruire nuovi indici per avere più variabili####
####ChatGPT - Qualità della Fermentazione####

w <- data.frame(TotalAcidity=white$fixed.acidity+white$volatile.acidity+white$citric.acid, 
                SugarAlcohol = white$residual.sugar-white$alcohol, 
                SulphurDioxideIndex = white$free.sulfur.dioxide+white$chlorides, 
                pHSulphates = white$pH-white$sulphates, 
                DensityClarity = white$density-white$chlorides, 
                Fermentability = (white$residual.sugar-white$volatile.acidity)/white$alcohol, 
                OxidationPotential = white$volatile.acidity*white$free.sulfur.dioxide,
                FlavorIntensity = white$volatile.acidity+white$citric.acid+white$sulphates, 
                BalanceAroma = white$volatile.acidity-white$sulphates, 
                SugarAcid = white$residual.sugar/white$fixed.acidity,
                #AcidityBalance =  (white$fixed.acidity^2)/white$citric.acid, 
                SweetnessBody = sqrt(white$residual.sugar)*(white$density^2),
                SulphurInteraction = (white$free.sulfur.dioxide*white$chlorides)+(white$free.sulfur.dioxide^2),
                AlcohlpH = white$alcohol/(white$pH^2),
                Complexity = exp(white$free.sulfur.dioxide)*log(1+white$volatile.acidity))

###Bootstrap anova###

values <- rep(0, dim(w)[2])
for (i in 1:1000)
{
  set.seed(534*i)
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

w <- data.frame(AcidRatio = (white$fixed.acidity)/(white$volatile.acidity),
                SugarAlcoholRatio = white$residual.sugar / white$alcohol, 
                SulphurHeatRatio = white$free.sulfur.dioxide / white$alcohol, 
                ChloridePHRatio = white$chlorides / white$pH, 
                CitricAcidRatio = white$citric.acid / (white$fixed.acidity + white$volatile.acidity), 
                AcidInteraction = white$fixed.acidity * white$volatile.acidity, 
                AlcoholSugarSquare = (white$alcohol)^2,
                pHSulphateInteraction = white$pH * white$sulphates, 
                AcidAlcoholInteraction = white$fixed.acidity * white$alcohol, 
                SugarChlorideInteraction = white$residual.sugar * white$chlorides,
                AcidRatioSquare = (white$fixed.acidity / white$volatile.acidity)^2,
                DensityAlcoholInteraction = white$density * white$alcohol,
                SulphurSugarInteraction = white$free.sulfur.dioxide * white$residual.sugar,
                AcidPHInteraction = white$fixed.acidity * white$pH)
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

Acidity.Index = (white$fixed.acidity + white$volatile.acidity) - white$citric.acid
w <- data.frame(Acidity.Index = Acidity.Index,
                Sugar2Acid.Ratio = white$residual.sugar / Acidity.Index,
                SulfurDioxide2ChlorideRatio = white$free.sulfur.dioxide / white$chlorides,
                Alcohol2Density.Ratio = white$alcohol/white$density,
                Sulphates2pH.Ratio = white$sulphates/white$pH,
                TotalAcidity = white$fixed.acidity + white$volatile.acidity + white$citric.acid,
                Chloride2Sulfate.Ratio = white$chlorides / white$sulphates,
                AlcoholContent = white$alcohol,
                SugarContent = white$residual.sugar,
                pHLevel = white$pH,
                AcidityInteraction = white$fixed.acidity * white$volatile.acidity,
                SugarSquared = white$residual.sugar^2,
                ChlorideCubed = (white$free.sulfur.dioxide^2)/white$sulphates,
                pHCubed = white$pH^3)

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

####TopGPT####
#TOP territorio: 
# Alcohol2Density.Ratio, AlcoholContent, SulfurDioxide2ChlorideRatio, 
# Chloride2Sulfate.Ratio, AcidityInteraction

#TOP qlty:
# AlcohlpH, SugarAlcohol, DensityClarity,...Fermentability

#TOP ferm:
# AlcoholSugarSquare, DensityAlcoholInteraction, ChloridePHRatio, 
# AcidAlcoholInteraction, SugarChlorideInteraction, SugarAlcoholRatio
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
good <- ifelse(rating=='7'|rating=='8'|rating=='9', 1, 0)
good <- as.factor(good)
i0 <- which(good=='0')
i1 <- which(good=='1')
try <- scale(w)
pca <- princomp(try)
summary(pca)

####Raw variables####
w <- white
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

