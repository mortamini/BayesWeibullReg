library(readxl)
library(data.table)
library(survival)
library(glmnet)

setwd(choose.dir())

x <- read_excel("impute with average.xlsx")
y <- read_excel("Y-te.xlsx")

x <- as.data.frame(x)
#set.seed(123)
#trainIndex <- sample(1:nrow(y),trunc(0.8*nrow(y)))
#save(trainIndex,file="trainindex.Rdata")
load("trainindex.Rdata")

trainData <- x[trainIndex,]
testData  <- x[-trainIndex,]
#======================================================
# Convert 'trainData' and 'testData' to a matrix suitable for glmnet
par(mfrow = c(2,4))
x_train <- glmnet::makeX(trainData, na.impute = TRUE)
x_test <- glmnet::makeX(testData, na.impute = TRUE)

# Convert 'y' to a data frame
if (!is.data.frame(y)) {
  y <- as.data.frame(y)
}

y_train <- y[trainIndex,]
y_test <- y[-trainIndex,]

if (nrow(x_train) != nrow(y_train)) {
  stop("The number of rows in 'x_train' must match the length of 'y_train'.")
}

# Convert 'y' to survival object
if (!inherits(y_train, "Surv")) {
  y_train <- with(y_train, Surv(time,status))
}


set.seed(1)
cvfit <- cv.glmnet(x_train, y_train, family = "cox", type.measure = "C")
plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se
cindex1 = cvfit$cvm[cvfit$index[1]]

lambda.min = cvfit$lambda.min

# Refit the model using lambda.min
fit = glmnet(x_train, y_train,family = "cox", lambda = lambda.min)

print(fit)

# Get the coefficients at lambda.min
coefs <- coef(fit, s = lambda.min)

#predictions on the test data
predictions <- predict(fit, newx = x_test, type = "response")

# Calculate the accuracy of the predictions

library(survival)

if (!inherits(y_test, "Surv")) {
  y_test <- with(y_test, Surv(time,status))
}

mse1 = mean((y_test[,1] - predictions)^2)
p1 = ncol(x_train)
#====================================================

# Calculate the c_index
#concordance_index <- concordance(y_test ~ predictions)$concordance
#cat("Concordance Index: ", concordance_index,'\n')


vars <- sapply(1:ncol(x_train),function(i){var(x_train[,i])})
cors <- sapply(1:ncol(x_train),function(i){cor(y_train[,1],x_train[,i])})
pvar <- quantile(vars,0.25)
pcor <- quantile(cors,0.25)
x_train <- x_train[,(vars>pvar & cors>pcor)]
x_test <- x_test[,(vars>pvar & cors>pcor)]

set.seed(1)
cvfit <- cv.glmnet(x_train, y_train, family = "cox", type.measure = "C")
plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se
cindex2 = cvfit$cvm[cvfit$index[1]]

lambda.min = cvfit$lambda.min

# Refit the model using lambda.min
fit = glmnet(x_train, y_train,family = "cox", lambda = lambda.min)

print(fit)

# Get the coefficients at lambda.min
coefs <- coef(fit, s = lambda.min)

#predictions on the test data
predictions <- predict(fit, newx = x_test, type = "response")

# Calculate the accuracy of the predictions

library(survival)

if (!inherits(y_test, "Surv")) {
  y_test <- with(y_test, Surv(time,status))
}

mse2 = mean((y_test[,1] - predictions)^2)
p2 = ncol(x_train)
#====================================================



vars <- sapply(1:ncol(x_train),function(i){var(x_train[,i])})
cors <- sapply(1:ncol(x_train),function(i){cor(y_train[,1],x_train[,i])})
pvar <- quantile(vars,0.25)
pcor <- quantile(cors,0.25)
x_train <- x_train[,(vars>pvar & cors>pcor)]
x_test <- x_test[,(vars>pvar & cors>pcor)]


#fit = glmnet(x_train, y_train,family = "cox", nlambda = 100,alpha = 0.5)

#coefs <- coef(fit)
#mostdfcoef <- coefs[,ncol(coefs)]

#x_train <- x_train[,mostdfcoef != 0]
#x_test <- x_test[,mostdfcoef != 0]


set.seed(1)
cvfit <- cv.glmnet(x_train, y_train, family = "cox", type.measure = "C")
plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se
cindex3 = cvfit$cvm[cvfit$index[1]]

lambda.min = cvfit$lambda.min

# Refit the model using lambda.min
fit = glmnet(x_train, y_train,family = "cox", lambda = lambda.min)

print(fit)

# Get the coefficients at lambda.min
coefs <- coef(fit, s = lambda.min)

#predictions on the test data
predictions <- predict(fit, newx = x_test, type = "response")

# Calculate the accuracy of the predictions

library(survival)

if (!inherits(y_test, "Surv")) {
  y_test <- with(y_test, Surv(time,status))
}

mse3 = mean((y_test[,1] - predictions)^2)
p3 = ncol(x_train)
#====================================================



vars <- sapply(1:ncol(x_train),function(i){var(x_train[,i])})
cors <- sapply(1:ncol(x_train),function(i){cor(y_train[,1],x_train[,i])})
pvar <- quantile(vars,0.25)
pcor <- quantile(cors,0.25)
x_train <- x_train[,(vars>pvar & cors>pcor)]
x_test <- x_test[,(vars>pvar & cors>pcor)]


#fit = glmnet(x_train, y_train,family = "cox", nlambda = 100,alpha = 0.5)

#coefs <- coef(fit)
#mostdfcoef <- coefs[,ncol(coefs)]

#x_train <- x_train[,mostdfcoef != 0]
#x_test <- x_test[,mostdfcoef != 0]


set.seed(1)
cvfit <- cv.glmnet(x_train, y_train, family = "cox", type.measure = "C")
plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se
cindex4 = cvfit$cvm[cvfit$index[1]]

lambda.min = cvfit$lambda.min

# Refit the model using lambda.min
fit = glmnet(x_train, y_train,family = "cox", lambda = lambda.min)

print(fit)

# Get the coefficients at lambda.min
coefs <- coef(fit, s = lambda.min)

#predictions on the test data
predictions <- predict(fit, newx = x_test, type = "response")

# Calculate the accuracy of the predictions

library(survival)

if (!inherits(y_test, "Surv")) {
  y_test <- with(y_test, Surv(time,status))
}

mse4 = mean((y_test[,1] - predictions)^2)
p4 = ncol(x_train)
#====================================================


vars <- sapply(1:ncol(x_train),function(i){var(x_train[,i])})
cors <- sapply(1:ncol(x_train),function(i){cor(y_train[,1],x_train[,i])})
pvar <- quantile(vars,0.25)
pcor <- quantile(cors,0.25)
x_train <- x_train[,(vars>pvar & cors>pcor)]
x_test <- x_test[,(vars>pvar & cors>pcor)]


#fit = glmnet(x_train, y_train,family = "cox", nlambda = 100,alpha = 0.5)

#coefs <- coef(fit)
#mostdfcoef <- coefs[,ncol(coefs)]

#x_train <- x_train[,mostdfcoef != 0]
#x_test <- x_test[,mostdfcoef != 0]


set.seed(1)
cvfit <- cv.glmnet(x_train, y_train, family = "cox", type.measure = "C")
plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se
cindex5 = cvfit$cvm[cvfit$index[1]]

lambda.min = cvfit$lambda.min

# Refit the model using lambda.min
fit = glmnet(x_train, y_train,family = "cox", lambda = lambda.min)

print(fit)

# Get the coefficients at lambda.min
coefs <- coef(fit, s = lambda.min)

#predictions on the test data
predictions <- predict(fit, newx = x_test, type = "response")

# Calculate the accuracy of the predictions

library(survival)

if (!inherits(y_test, "Surv")) {
  y_test <- with(y_test, Surv(time,status))
}

mse5 = mean((y_test[,1] - predictions)^2)
p5 = ncol(x_train)
#====================================================

plot(1:5,c(cindex1,cindex2,cindex3,cindex4,cindex5),ylim = c(0,1),
xlab = "phase",ylab="c-index",type="o",pch=16,col="red")


plot(1:5,c(mse1,mse2,mse3,mse4,mse5),ylim = c(100,200),
xlab = "phase",ylab="MSPE",type="o",pch=16,col="blue")

plot(1:5,c(p1,p2,p3,p4,p5),
xlab = "phase",ylab="dimension",type="o",pch=16,col="green")

#====================================================

save(x_train,y_train,x_test,y_test,file="data.Rdata")

cindexes = c(cindex1,cindex2,cindex3,cindex4,cindex5)
mspes = c(mse1,mse2,mse3,mse4,mse5)
dims = c(p1,p2,p3,p4,p5)
save(cindexes,mspes,dims, file = "glmnetresults.Rdata")



