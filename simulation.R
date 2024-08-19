# install.packages("coxed")
library(coxed)
library(survival)
library(glmnet)
library(rjags)
library(coda)
setwd(choose.dir())
source("glmnet.R")
source("bayescox.R")
source("vbcoxnet.R")
B <- 100
mseg <- mseb <- msevb <- 
fprg <- fnrg <- 
fprb <- fnrb <- 
fprvb <- fnrvb <- 
betaeg <- betaeb <- betaevb <- c()
for(iter in 1:B){
	cat("Iteration",iter,"\n")
	beta <- rnorm(10,0,0.1)
	z <- rbinom(10,1,0.3)
	beta <- beta * z
	train <- sim.survdata(N = 80, T = 50, 
		beta = beta, xvars = 10, mu = 0,
		sd = 0.3, covariate = 1, censor = 0.1)
	test <- sim.survdata(N = 20, T = 50, 
		beta = beta, xvars = 10, mu = 0,
		sd = 0.3, covariate = 1, censor = 0.1)
	gmodel <- myglmnet(train,test)
	bmodel <- bayescox(train,test)
	vbmodel <- vbcoxnet(train,test)
	mseg <- c(mseg,gmodel$mse)
	mseb <- c(mseb,bmodel$mse)
	msevb <- c(msevb,vbmodel$mse)
	betag <- as.numeric(gmodel$betahat)
	betab <- bmodel$betahat
	betavb <- vbmodel$betahat
	betaeg <- c(betaeg, (beta-betag)^2)
	betaeb <- c(betaeb, (beta-betab)^2)
	betaevb <- c(betaevb, (beta-betavb)^2)
	fnrg <- c(fnrg, sum(beta != 0 & betag == 0)/sum(beta != 0))
	fnrb <- c(fnrb, sum(beta != 0 & betab == 0)/sum(beta != 0))
	fnrvb <- c(fnrvb, sum(beta != 0 & betavb == 0)/sum(beta != 0))
	fprg <- c(fprg, sum(beta == 0 & betag != 0)/sum(beta == 0))
	fprb <- c(fprb, sum(beta == 0 & betab != 0)/sum(beta == 0))
	fprvb <- c(fprvb, sum(beta == 0 & betavb != 0)/sum(beta == 0))
}
par(mfrow = c(2,2))
mse <- cbind(mseg,mseb,msevb)
colnames(mse) <- c("coxnet","Bayes","VB")
boxplot(mse,main = "MSE")
betae <- cbind(betaeg,betaeb,betaevb)
colnames(betae) <- c("coxnet","Bayes","VB")
boxplot(betae,main = "Beta error")
fpr <- cbind(fprg,fprb,fprvb)
colnames(fpr) <- c("coxnet","Bayes","VB")
boxplot(fpr,main = "FPR")
fnr <- cbind(fnrg,fnrb,fnrvb)
colnames(fnr) <- c("coxnet","Bayes","VB")
boxplot(fnr,main = "FNR")





