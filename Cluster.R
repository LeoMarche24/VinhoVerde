setwd("C:/Users/leoma/OneDrive/Documents/PoliMi/Applied Statistics/R/Progetto")
rm(list=ls())
white <- read.csv("winequality-white.csv", header=T, sep=";")
rating <- white[, 12]
white[, 6] <- white[, 6]/white[, 7]
white <- white[, -7]
white <- white[, -11]
red <- read.csv("winequality-red.csv", header=T, sep=";")
rating <- c(rating,red[, 12])
red[, 6] <- red[, 6]/red[, 7]
red <- red[, -7]
red <- red[, -11]

w <- rbind(white, red)
wr <- c(rep('darkgreen', dim(white)[1]), rep('darkred', dim(red)[1]))
pca <- princomp(scale(w))
x11()
plot(pca$scores[, 1:2], col=wr, pch=16)
score <- pca$scores[, 1:2]
summary(pca)
pca$loadings

###Sono influenti per i rating?###
lines <- which(rating!=6)
plot(pca$scores[lines, 1:2], col=ifelse(rating[lines]<6, '1','2'), pch=16)

dis.mat <- dist(score, method='euclidean')
clust.ea <- hclust(dis.mat, method='ward.D')
x11()
plot(clust.ea, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

cluster.ec <- cutree(clust.ea, k=2)
levels(as.factor(cluster.ec))

plot(score, col=cluster.ec+2)
mean(cluster.ec != as.factor(ifelse(wr=='darkgreen', 1, 2)))
