library(rjags)

load("/mnt/data.Rdata")

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

R <- rep(2,nc)
cut <- y[1:nc]
y[1:nc] <- NA
lim <- matrix(0,nc,2)
for (i in 1:nc){
  lim[i,] <- c(-Inf,cut[i]+1)
}

data_list <- list(
  x = x,
  y = y,
  R = R,
  lim = lim,
  z = z,
  nc = nc,
  no = no,
  nt = nt, 
  xt = xt, 
  zt = zt, 
  J = ncol(x),
  Q = ncol(z),
  C = c(1e-300,1e-12)
)
  
jags_model = "model{
	for (i in 1:nc){
		R[i] ~ dinterval(y[i],lim[i,])
    	y[i] ~ dweib(alpha, b[i])
    	b[i] <- exp(-mu - inprod(x[i,], beta) - inprod(z[i,], delta) - 0.57721/alpha)
	}
	for (i in (nc+1):no){
    	y[i] ~ dweib(alpha, b[i])
    	b[i] <- exp(-mu - inprod(x[i,], beta) - inprod(z[i,], delta) - 0.57721/alpha)
	}
	for (i in 1:nt){
    	yhat[i] ~ dweib(alpha, bhat[i])
    	bhat[i] <- exp(-mu - inprod(xt[i,], beta) - inprod(zt[i,], delta) - 0.57721/alpha)
	}
    	# Prior for alpha
	alpha ~ dgamma(alpha0, kappa0)
    	# Priors for beta
   	for (j in 1:J) {
      	beta[j] ~ dnorm(0, pow(C[gamma[j]] * sigmaG2, -1))
	}
    	# Prior for sigmaG^2
      tauG ~ dgamma(alpha_sigma, beta_sigma)
	  sigmaG2 <- pow(tauG,-1)
    	#Prior for delta[q] 
      for (q in 1:Q) {
		delta[q] ~ dnorm(0, tau)
      }
      tau <- pow(sigma2_delta, -1)
    	#priors for mu
      mu ~ dnorm(0, tau_mu)
      tau_mu <- pow(sigma2_mu, -1)
      #priors for gamma[j]
      for (j in 1:J){
      	gamma[j] ~ dcat(pi[1:2])
	  }
   	pi[1:2] ~ ddirch(alphapi[1:2]) 
	alpha0 <- 0.0001
	kappa0 <- 0.0001
	alpha_sigma <- 0.0001
	beta_sigma <- 0.0001
	sigma2_delta <- 100000
	sigma2_mu <- 100000
	alphapi[1] <- 1.001
	alphapi[2] <- 1.001
}"


param <-
  c("alpha",
    "beta",
    "tauG",
    "delta",
    "mu",
    "gamma",
	"pi",
	"yhat")

 inits=function(){
  inits=list("alpha"=runif(1),
    "beta"=rnorm(ncol(x),0,1e-12),
    "tauG"=1,
    "delta"=rnorm(ncol(z),0,1e-12),
    "mu"=0,
    "gamma"=rep(2,ncol(x)),
	"pi"=rep(0.5,2)
  )
 }
            
 
 model <- 
jags.model(textConnection(jags_model), 
data = data_list, 
inits = inits,n.chain=1)

library(coda)
update(model,10000)
mod_sim=coda.samples(model = model,
	variable.names = param,n.iter = 30000,thin=30)
#-----------
theta = as.matrix(mod_sim)

save(theta,file = "/mnt/BayesOutput.Rdata")
cat("Done\n") 
