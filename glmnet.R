myglmnet <- function(train,test){
	x_train <- glmnet::makeX(train$xdata)
	x_test <- glmnet::makeX(test$xdata)
	nx <- ncol(train$xdata)
	y_train = train$data[,-(1:nx)]
	if (!is.data.frame(y_train)) {
  		y_train <- as.data.frame(y_train)
	}
	y_test = test$data[,-(1:nx)]
	if (!is.data.frame(y_test)) {
  		y_test <- as.data.frame(y_test)
	}
	if (!inherits(y_train, "Surv")) {
  		y_train <- with(y_train, Surv(y,failed))
	}
	cvfit <- cv.glmnet(x_train, y_train, family = "cox", type.measure = "C")
	lambda.min = cvfit$lambda.min
	fit = glmnet(x_train, y_train,family = "cox", lambda = lambda.min)
	coefs <- coef(fit, s = lambda.min)
	predictions <- predict(fit, newx = x_test, type = "response")
	mse = mean((y_test[,1] - predictions)^2)
	betahat = coefs
	list(mse = mse, betahat = betahat)
}