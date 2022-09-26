scalar_predict <- function(object, newx, s=c("lambda.1se","lambda.min"), type){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[['lambda']])
  if(type[1]=="response"){
    coef <- object$betas[,index]
    output <- as.numeric(cbind(1,newx) %*% coef)
  } else {
    coef <- object$betas[,index]
    output <- coef  
  }  
  return(output)
}

#object <- cv.model;s=c("lambda.min");type = c("response");newx <- X.data


predict.cv.rbridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                              type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[['lambda']])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}





coef.cv.rbridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}




predict.rbridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                              type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[['lambda']])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}


coef.rbridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}


#### Bridge
predict.cv.bridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                              type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[[5]])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}





coef.cv.bridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}




predict.bridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                           type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[[2]])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}


coef.bridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}

