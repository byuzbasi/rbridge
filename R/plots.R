######################################################################
## These functions are minor modifications or directly copied from the
## glmnet package:
##        Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
## Regularization Paths for Generalized Linear Models via Coordinate
#   Descent.
##        Journal of Statistical Software, 33(1), 1-22.
##        URL http://www.jstatsoft.org/v33/i01/.
## The reason they are copied here is because they are internal functions
## and hence are not exported into the global environment.
## The original comments and header are preserved.
er.bars <-
  function(x, upper, lower, width = 0.02, ...)
  {
    xlim <- range(x)
    barw <- diff(xlim) * width
    segments(x, upper, x, lower, ...)
    segments(x - barw, upper, x + barw, upper, ...)
    segments(x - barw, lower, x + barw, lower, ...)
    range(upper, lower)
  }

plot.cv.rbridge=function(object,sign.lambda=1,...){
  xlab="log(Lambda)"
  if(sign.lambda<0)xlab=paste("-",xlab,sep="")
  plot.args=list(x=sign.lambda*log(object$lambda),
                 y=object$cve,ylim=range(object$cvup,object$cvlo),
                 xlab=xlab,ylab='Mean-Squared Error',type="n")
  new.args <- list(...)
  if (length(new.args)) 
    plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  er.bars(sign.lambda*log(object$lambda),object$cvup,object$cvlo,width=0.01,col="darkgrey")
  points(sign.lambda*log(object$lambda),object$cve,pch=20,col="red")
  axis(side=3,at=sign.lambda*log(object$lambda),labels=paste(object$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(object$lambda.min),lty=3)
  abline(v=sign.lambda*log(object$lambda.1se),lty=3)
  invisible()
}


### Bridge
plot.cv.bridge=function(object,sign.lambda=1,...){
  xlab="log(Lambda)"
  if(sign.lambda<0)xlab=paste("-",xlab,sep="")
  plot.args=list(x=sign.lambda*log(object$lambda),
                 y=object$cve,ylim=range(object$cvup,object$cvlo),
                 xlab=xlab,ylab='Mean-Squared Error',type="n")
  new.args <- list(...)
  if (length(new.args)) 
    plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  er.bars(sign.lambda*log(object$lambda),object$cvup,object$cvlo,width=0.01,col="darkgrey")
  points(sign.lambda*log(object$lambda),object$cve,pch=20,col="red")
  axis(side=3,at=sign.lambda*log(object$lambda),labels=paste(object$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(object$lambda.min),lty=3)
  abline(v=sign.lambda*log(object$lambda.1se),lty=3)
  invisible()
}


