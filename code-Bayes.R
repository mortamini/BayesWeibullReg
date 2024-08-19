#install.packages("rjags")
library(rjags)
library(readxl)
library(data.table)

setwd(choose.dir())

xz <- read_excel("impute with average.xlsx")
yd <- read_excel("Y-te.xlsx")


x <- as.matrix(xz[,-1])
z <- as.matrix(xz[,1])
d <- as.vector(unlist(yd[,2]))
y <- as.vector(unlist(yd[,1]))

x <- x[order(d),]
z <- z[order(d),,drop=F]
y <- y[order(d)]
no <- sum(d == 1)
nc <- sum(d == 0)

n <- nrow(x)

R <- rep(2,nc)
cut <- y[1:nc]
y[1:nc] <- NA
lim <- matrix(0,nc,2)
for (i in 1:nc){
  lim[i,] <- c(-Inf,16.1)
}


data_list <- list(
  x = x,
  y = y,
  R = R,
  lim = lim,
  z = z,
  nc = nc,
  no = no,
  J = ncol(x),
  Q = ncol(z),
  C = c(0.000000000001,0.0001,0.001,0.01)
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
      	gamma[j] ~ dcat(pi[1:4])
	  }
   	pi[1:4] ~ ddirch(alphapi[1:4]) 
	alpha0 <- 0.0001
	kappa0 <- 0.0001
	alpha_sigma <- 1.0001
	beta_sigma <- 0.0001
	sigma2_delta <- 100000
	sigma2_mu <- 100000
	alphapi[1] <- 1.001
	alphapi[2] <- 1.001
	alphapi[3] <- 1.001
	alphapi[4] <- 1.001
}"


param <-
  c("alpha",
    "beta",
    "tauG",
    "delta",
    "mu",
    "gamma",
	"pi")



 inits=function(){
  inits=list("alpha"=runif(1),
    "beta"=rnorm(ncol(x)),
    "tauG"=1,
    "delta"=rnorm(ncol(z)),
    "mu"=0,
    "gamma"=rep(2,ncol(x)),
	"pi"=rep(0.25,4)
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
 