predict.abcrf <- function(object, sumsta, obs, ...)
{
	if (is.vector(obs)) obs=matrix(obs,1,length(obs))
	if (is.null(colnames(sumsta))) {
	  colnames(sumsta)= paste("V",1:dim(sumsta)[2],sep="")
	  colnames(obs)=colnames(sumsta)
	} 
	if (is.null(colnames(obs))) {
	  colnames(obs)=colnames(sumsta)
    warning("Columns of obs have no names")
	}
	old.options <- options(); options(warn=-1)
	if (object$lda) {
		obs <- cbind(obs, predict(object$model.lda ,obs)$x)
		sumsta <- cbind(sumsta, predict(object$model.lda ,sumsta)$x)
	}
	allocation <- predict(object$model.rf, obs)
	local.error <- as.numeric(object$model.rf$predicted==object$model.rf$y)
	error.rf <- randomForest(sumsta, local.error, ...)
	options(old.options)
	tmp <- list(allocation=allocation,post.prob=predict(error.rf, obs))
  class(tmp) <- "abcrfpredict"
  tmp
}

summary.abcrfpredict <- function(object, ...) {
  cat("Number of affectations per model:\n")
  summary(object$allocation, ...)
}

print.abcrfpredict <- function(x, ...) {
  ret <- cbind(x$allocation, x$post.prob)
  colnames(ret) <- c("model", "post.proba")
  print(ret, ...)
}

as.matrix.abcrfpredict <- function(x, ...) {
  ret <- cbind(x$allocation, x$post.prob)
  colnames(ret) <- c("model", "post.proba")
  ret
}

as.data.frame.abcrfpredict <- function(x, ...) {
  ret <- cbind(x$allocation, x$post.prob)
  colnames(ret) <- c("model", "post.proba")
  as.data.frame(ret,  row.names=NULL, optional=FALSE, ...)
}

as.list.abcrfpredict <- function(x, ...) {
  list(allocation = x$allocation,  post.prob = x$post.prob, ...)
}