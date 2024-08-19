
load("BayesOutput.Rdata")
load("data.Rdata")

xz <- x_train
yd <- y_train

x <- as.matrix(xz[,-1])
z <- as.matrix(xz[,1])
d <- as.vector(unlist(yd[,2]))
y <- as.vector(unlist(yd[,1]))

xzt <- x_test
ydt <- y_test

xt <- as.matrix(xzt[,-1])
zt <- as.matrix(xzt[,1])
dt <- as.vector(unlist(ydt[,2]))
yt <- as.vector(unlist(ydt[,1]))

x <- x[order(d),]
z <- z[order(d),,drop=F]
y <- y[order(d)]
no <- sum(d == 1)
nc <- sum(d == 0)

n <- nrow(x)
nt <- nrow(xt)
yhat <- trunc(theta[1000,2473:2715])
yhat <- pmin(yhat,15)
mse = mean((yt - yhat)^2)
betahat = colMeans(theta[,2:1234])
gamma = sapply(1236:2468, function(i){
		as.numeric(names(table(theta[,i])))[which.max(table(theta[,i]))]
})
betahat = (gamma != 0) * betahat

save(mse,betahat,yhat,file="Bayesresults.Rdata")



