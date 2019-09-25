#Question 2
bears <- read.table("~/cofc/DATA550/Bears.csv",header=TRUE,sep=",")
attach(bears)

#Part 2.A
plot(ChestGirth, Weight, pch = 16, cex = 1.3, col = "blue", main = "Weight plotted against Chest Girth", xlab = "Chest Girth (inches)", ylab = "Weight (pounds)")
lm(Weight ~ ChestGirth)
abline(lm(Weight ~ ChestGirth))
modelA <- lm(Weight ~ ChestGirth)
  par(mfrow = c(2,2))
plot(modelA)

#Part 2.B
ChestGirth2 <- ChestGirth^2

plot(ChestGirth2, Weight, pch = 16, cex = 1.3, col = "blue", main = "Weight plotted against Chest Girth^2", xlab = "Chest Girth^2 (inches^2)", ylab = "Weight (pounds)")
lm(Weight ~ ChestGirth2)
abline(lm(Weight ~ ChestGirth2))
modelB <- lm(Weight ~ ChestGirth2)
par(mfrow = c(2,2))
plot(modelB)

#Part 2.C
logChestGirth <- log(ChestGirth)
logWeight <- log(Weight)

plot(logChestGirth, logWeight, pch = 16, cex = 1.3, col = "blue", main = "log(Weight) plotted against log(Chest Girth)", xlab = "Chest Girth (inches)", ylab = "Weight (pounds)")
lm(logWeight ~ logChestGirth)
abline(lm(logWeight ~ logChestGirth))
modelC <- lm(logWeight ~ logChestGirth)
par(mfrow = c(2,2))
plot(modelC)

#Part 2.D
#95% confidence intervals
round(confint(modelA,level=0.95),3)
#95% Prediction Interval
newdata = data.frame(ChestGirth=38)
predict(modelA, newdata, interval="predict") 

#Question 3
library(Stat2Data)
data(PalmBeach)
palm<-PalmBeach

#Part 3.A
palm.lm<-lm(Buchanan~Bush,data=palm)
plot(Buchanan~Bush,data=palm,col="purple",pch=19)
abline(palm.lm,lwd=2)

standres<-rstandard(palm.lm)
plot(standres~palm.lm$fitted.values,pch=19,col=12)
par(mfrow = c(2,2))
plot(palm.lm)

#Part 3.C
par(mfrow = c(1,1))
palmSubset <- palm[-c(50,13),]
palm.lm2<-lm(Buchanan~Bush,data=palmSubset)
plot(Buchanan~Bush,data=palmSubset,col="purple",pch=19)
abline(palm.lm2,lwd=2)

palmSubset.lm<-lm(log(Buchanan)~log(Bush),data=palmSubset)
plot(log(Buchanan)~log(Bush),data=palmSubset,col="purple",pch=19)
abline(palmSubset.lm,lwd=2)
par(mfrow = c(2,2))
plot(palmSubset.lm)

#Part 3.D
round(confint(palm.lm2,level=0.95),3)
#95% Prediction Interval
data(PalmBeach)
palm<-PalmBeach
newdata = data.frame(County="DADE")
predict(palm.lm2, newdata, interval="predict") 


