# Classification-of-Benign-and-Harmful-Substances
Codes for "Classification of Benign and Harmful Substances" Projects with R

anthrax1 <- read.csv("anthrax1.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
anthrax2 <- read.csv("anthrax1.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
anthrax3 <- read.csv("anthrax1.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)

armh <- read.csv("armh.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE)
bakingpowder <- read.csv("bakingpowder.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE)
bakingsoda <- read.csv("bakingsoda.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE)
crayolachalk <- read.csv("crayolachalk.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE)
flour <- read.csv("flour.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
ibuprofen <- read.csv("ibuprofen.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
sugar <- read.csv("sugar.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
tideldbaked <- read.csv("tideldbaked.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
tylenolgelcap <- read.csv("tylenolgelcap.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)

elements <- read.csv("elements2.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
test_cases <- read.csv("test_cases.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
wave <- scan("wave.txt")

# First, look at the distribution of wavelengths between anthrax and 9 substances.
anthrax <- rbind(anthrax1, anthrax2, anthrax3)
avg_anthrax <- apply(anthrax, 2, mean)

avg_armh <- apply(armh, 2, mean)
avg_bakingpowder <- apply(bakingpowder, 2, mean)
avg_bakingsoda <- apply(bakingsoda, 2, mean)
avg_crayolachalk <- apply(crayolachalk, 2, mean)
avg_flour <- apply(flour, 2, mean)
avg_ibuprofen <- apply(ibuprofen, 2, mean)
avg_sugar <- apply(sugar, 2, mean)
avg_tideldbaked <- apply(tideldbaked, 2, mean)
avg_tylenolgelcap <- apply(tylenolgelcap, 2, mean)

#par(mfrow = c(2,2))
#plot(wave, anthrax1[1,], type="l")
#plot(wave, anthrax2[1,], type="l")
#plot(wave, anthrax3[1,], type="l")

par(mfrow = c(3,3))
plot(wave, avg_armh, type="l", col = "red", main = "armh")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_bakingpowder, type="l", col = "red", main = "bakingpowder")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_bakingsoda, type="l", col = "red", main = "bakingsoda")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_crayolachalk, type="l", col = "red", main = "crayolachalk")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_flour, type="l", col = "red", main = "flour")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_ibuprofen, type="l", col = "red", main = "ibuprofen")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_sugar, type="l", col = "red", main = "sugar")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_tideldbaked, type="l", col = "red", main = "tideldbaked")
lines(wave, avg_anthrax, type = "l", col = "blue")
plot(wave, avg_tylenolgelcap, type="l", col = "red", main = "tylenolgelcap")
lines(wave, avg_anthrax, type = "l", col = "blue")

# It's hard to tell from the graph. Some subtances have very different patterns
# of wavelengths, while some subtances do not.

# PCA may help to reduce dimensions and help us construct some factors for future model.
x <- rbind(anthrax1, anthrax2, anthrax3, armh, bakingpowder, bakingsoda, crayolachalk,
           flour, ibuprofen, sugar, tideldbaked, tylenolgelcap)
pca <- prcomp(x, cor = F)
summary(pca)
par(mfrow = c(1,2))
plot(pca)
screeplot(pca, type = "lines")

# Look at how these points are seperated on different combinations of PC
labels0 <- c(rep(1,36), rep(0, 108))
par(mfrow = c(1,3))
plot(pca$x[,1], pca$x[,2], type = "n", xlab = "PCA 1", ylab = "PCA 2", pch = 0, main = "")
text(pca$x[,1], pca$x[,2], labels=labels0, col = labels0 +1, cex = 1.25)
plot(pca$x[,1], pca$x[,3], type = "n", xlab = "PCA 1", ylab = "PCA 3", pch = 0, main = "")
text(pca$x[,1], pca$x[,3], labels=labels0, col = labels0 +1, cex = 1.25)
plot(pca$x[,1], pca$x[,4], type = "n", xlab = "PCA 1", ylab = "PCA 4", pch = 0, main = "")
text(pca$x[,1], pca$x[,4], labels=labels0, col = labels0 +1, cex = 1.25)
legend("topleft", c("Benign (0)", "Bateria (1)"), text.col = 1:2)


# Based on the elbow point, choose k = 3
pc <- pca$x[,1:3]

# Look at the distribution of wavelengths between anthrax and 9 substances again.
avg_anthrax1 <- apply(pc[1:36,], 2, mean)
avg_armh1 <- apply(pc[37:48,], 2, mean)
avg_bakingpowder1 <- apply(pc[49:60,], 2, mean)
avg_bakingsoda1 <- apply(pc[61:72,], 2, mean)
avg_crayolachalk1 <- apply(pc[73:84,], 2, mean)
avg_flour1 <- apply(pc[85:96,], 2, mean)
avg_ibuprofen1 <- apply(pc[97:108,], 2, mean)
avg_sugar1 <- apply(pc[109:120,], 2, mean)
avg_tideldbaked1 <- apply(pc[121:132,], 2, mean)
avg_tylenolgelcap1 <- apply(pc[133:144,], 2, mean)

par(mfrow = c(3,3))
plot(1:3, avg_armh1, type="l", col = "red", main = "armh", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_bakingpowder1, type="l", col = "red", main = "bakingpowder", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_bakingsoda1, type="l", col = "red", main = "bakingsoda", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_crayolachalk1, type="l", col = "red", main = "crayolachalk", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_flour1, type="l", col = "red", main = "flour", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_ibuprofen1, type="l", col = "red", main = "ibuprofen", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_sugar1, type="l", col = "red", main = "sugar", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_tideldbaked1, type="l", col = "red", main = "tideldbaked", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")
plot(1:3, avg_tylenolgelcap1, type="l", col = "red", main = "tylenolgelcap", ylim = c(-9833.992, 42022.77))
lines(1:3, avg_anthrax1, type = "l", col = "blue")

# Some of them are good. But flour, ibuprofen, sugar and tylenolgelcap are still hard.

# Turn to elements: can we use the information in elements?
# Calculate avergae wavelengths for 5 elements.
e <- matrix(nrow = 9, ncol = 13701)
for (i in 1:9) {
  e[i,] <- apply(elements[(1+5*(i-1)):(5*i),-1], 2, median)
}
# Look at the distribution of wavelengths for elements.
par(mfrow = c(3,3))
for (i in 1:9) {
  plot(wave, e[i, ], type="l", main = i, ylab = "wavelengths median")
}

# Look at the coefficients of regression on 5 elements.
B2O3 <- elements[elements[,1] == "B2O3",-1 ]
graphite <- elements[elements[,1] == "graphite",-1 ]
CaClO3 <- elements[elements[,1] == "CaClO3",-1 ]
FeSO4 <- elements[elements[,1] == "FeSO4",-1 ]
KI <- elements[elements[,1] == "KI",-1 ]
MgSO4 <- elements[elements[,1] == "MgSO4",-1 ]
MnSO4 <- elements[elements[,1] == "MnSO4",-1 ]
NaCl <- elements[elements[,1] == "NaCl",-1 ]
Si <- elements[elements[,1] == "Si",-1 ]

B2O3.med <- apply(B2O3,2,median)
graphite.med <- apply(graphite,2,median)
CaClO3.med <- apply(CaClO3,2,median)
FeSO4.med <- apply(FeSO4,2,median)
KI.med <- apply(KI,2,median)
MgSO4.med <- apply(MgSO4,2,median)
MnSO4.med <- apply(MnSO4,2,median)
NaCl.med <- apply(NaCl,2,median)
Si.med <- apply(Si,2,median)

newx <- as.matrix(x)
lm.x <- sapply(1:nrow(newx), function(i) lm(newx[i,] ~ B2O3.med + graphite.med +
                                           CaClO3.med + FeSO4.med + KI.med +
                                           MgSO4.med + MnSO4.med + NaCl.med + 
                                           Si.med)$coefficients)

corr_anthrax <- apply(lm.x[2:10, 1:36], 1, median)
corr_armh <- apply(lm.x[2:10, 37:48], 1, median)
corr_bakingpowder <- apply(lm.x[2:10, 49:60], 1, median)
corr_bakingsoda <- apply(lm.x[2:10, 61:72], 1, median)
corr_crayolachalk <- apply(lm.x[2:10, 73:84], 1, median)
corr_flour <- apply(lm.x[2:10, 85:96], 1, median)
corr_ibuprofen <- apply(lm.x[2:10, 97:108], 1, median)
corr_sugar <- apply(lm.x[2:10, 109:120], 1, median)
corr_tideldbaked <- apply(lm.x[2:10, 121:132], 1, median)
corr_tylenolgelcap <- apply(lm.x[2:10, 133:144], 1, median)

# Look at the plot of correlations.
par(mfrow = c(3,3))
plot(1:9, corr_armh, type="l", col = "red", main = "armh")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_bakingpowder, type="l", col = "red", main = "bakingpowder")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_bakingsoda, type="l", col = "red", main = "bakingsoda")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_crayolachalk, type="l", col = "red", main = "crayolachalk")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_flour, type="l", col = "red", main = "flour")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_ibuprofen, type="l", col = "red", main = "ibuprofen")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_sugar, type="l", col = "red", main = "sugar")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_tideldbaked, type="l", col = "red", main = "tideldbaked")
lines(1:9, corr_anthrax, type = "l", col = "blue")
plot(1:9, corr_tylenolgelcap, type="l", col = "red", main = "tylenolgelcap")
lines(1:9, corr_anthrax, type = "l", col = "blue")

# Correlations work well for those four substances. 
# Add these 2nd and 4th correlations to model.


################## modelling 
# First, seperate the data into training set and testing set as 2:1.
train1 <- sample(1:36, 24, replace = FALSE)
train0 <- sample(37:144, 72, replace = FALSE)

xtrain <- x[c(train1, train0),]
ytrain <- c(rep(1, 24), rep(0, 72))
xtest <- x[-c(train1, train0),]
ytest <- c(rep(1, 12), rep(0, 36))

# First, doing PCA to xtrain and get PC1, PC2, PC3
pca <- prcomp(xtrain, cor = F)
pc <- pca$x[,1:3]

# Second, calculate coefficients.
newxtrain <- as.matrix(xtrain)
lm.xtrain <- sapply(1:nrow(newxtrain), function(i) lm(newxtrain[i,] ~ B2O3.med + graphite.med +
                                              CaClO3.med + FeSO4.med + KI.med +
                                              MgSO4.med + MnSO4.med + NaCl.med + 
                                              Si.med)$coefficients)

coeff <- t(lm.xtrain[c(3, 5),])

xtrainnew <- cbind(pc, coeff)
library(e1071)
set.seed(123)
svm1 <- svm(xtrainnew, as.factor(ytrain))
svm1.class <- as.numeric(predict(svm1,xtrainnew))-1
table(ytrain, svm1.class)

# testing.
temp <- scale(xtest, scale = FALSE)%*%pca$rotation[,1:3]
newxtest <- as.matrix(xtest)
lm.xtest <- sapply(1:nrow(newxtest), function(i) lm(newxtest[i,] ~ B2O3.med + graphite.med +
                                                        CaClO3.med + FeSO4.med + KI.med +
                                                        MgSO4.med + MnSO4.med + NaCl.med + 
                                                        Si.med)$coefficients)
xtestnew <- cbind(temp, t(lm.xtest[c(3, 5),]))
svmtest.class <- as.numeric(predict(svm1,xtestnew))-1
table(ytest, svmtest.class)

# new prediction
temp <- scale(test_cases, scale = FALSE)%*%pca$rotation[,1:3]
newtest_cases <- as.matrix(test_cases)
lm.newtest_cases <- sapply(1:nrow(newtest_cases), 
                           function(i) lm(newtest_cases[i,] ~ B2O3.med + graphite.med +
                                                      CaClO3.med + FeSO4.med + KI.med +
                                                      MgSO4.med + MnSO4.med + NaCl.med + 
                                                      Si.med)$coefficients)
xnewtest_cases <- cbind(temp, t(lm.newtest_cases[c(3, 5),]))
as.numeric(predict(svm1,xnewtest_cases))-1
