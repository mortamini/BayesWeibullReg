bayescox <- function(train, test){
	x <- train$xdata
	xtest <- test$xdata
	d <- train$data$failed * 1
	y <- train$data$y
	x <- x[order(d),]
	y <- y[order(d)]
	d <- d[order(d)]
	no <- sum(d == 1)
	nc <- sum(d == 0)
	n <- nrow(x)
	R <- rep(2,nc)
	cut <- y[1:nc]
	y[1:nc] <- NA
	lim <- matrix(0,nc,2)
	for (i in 1:nc){
  		lim[i,] <- c(-Inf,cut[i]+1)
	}
	data_list <- list(
  		x = x,
		xtest = xtest,
  		y = y,
  		R = R,
  		lim = lim,
	  	nc = nc,
  		no = no,
  		J = ncol(x),
  		C = c(1e-300,1)
	)
	jags_model = "model{
		for (i in 1:nc){
			R[i] ~ dinterval(y[i],lim[i,])
    		y[i] ~ dweib(alpha, b[i])
    		b[i] <- exp(-mu - inprod(x[i,], beta) - 0.57721/alpha)
		}
		for (i in (nc+1):no){
    		y[i] ~ dweib(alpha, b[i])
    		b[i] <- exp(-mu - inprod(x[i,], beta) - 0.57721/alpha)
		}
		for (i in 1:20){
    		yhat[i] ~ dweib(alpha, bhat[i])
    		bhat[i] <- exp(-mu - inprod(xtest[i,], beta) - 0.57721/alpha)
		}
    	# Prior for alpha
		alpha ~ dgamma(alpha0, kappa0)
    	# Priors for beta
   		for (j in 1:J) {
      		beta[j] ~ dnorm(0, pow(C[gamma[j]+1] * sigmaG2, -1))
		}
    	# Prior for sigmaG^2
      	tauG ~ dgamma(alpha_sigma, beta_sigma)
	  	sigmaG2 <- pow(tauG,-1)
    	#priors for mu
      	mu ~ dnorm(0, tau_mu)
      	tau_mu <- pow(sigma2_mu, -1)
      	#priors for gamma[j]
      	for (j in 1:J){
      		gamma[j] ~ dbern(pi)
	  	}
   		pi ~ dbeta(alphapi,betapi) 
		alpha0 <- 0.0001
		kappa0 <- 0.0001
		alpha_sigma <- 0.0001
		beta_sigma <- 0.0001
		sigma2_mu <- 100000
		alphapi <- 1
		betapi <- 1
	}"
	param <-
  	c("alpha",
    "beta",
    "tauG",
    "mu",
    "gamma",
	"pi",
	"yhat")
 	inits=function(){
  		inits=list("alpha"=runif(1),
    "beta"=rnorm(ncol(x)),
    "tauG"=1,
    "mu"=0,
    "gamma"=rep(1,ncol(x)),
	"pi"=0.5
  	)
 	}
 	model <- 
		jags.model(textConnection(jags_model), 
		data = data_list, 
		inits = inits,n.chain=1)
	update(model,5000)
	mod_sim=coda.samples(model = model,
		variable.names = param,n.iter = 10000,thin=10)
	theta = as.matrix(mod_sim)
	yhat <- trunc(theta[1000,25:44])
	yhat <- pmin(yhat,50)
	mse = mean((test$data$y - yhat)^2)
	betahat = colMeans(theta[,2:11])
	gamma = sapply(12:21, function(i){
		as.numeric(names(table(theta[,i])))[which.max(table(theta[,i]))]
	})
	betahat = (gamma != 0) * betahat
	list(mse = mse, betahat = betahat)
}

