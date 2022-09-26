#The function to solve bridge regression
bridge_R<-function(x, y, lambda, q=1, eta=1e-7){
  library(glmnet)
  # use ridge coefficients as a starting value
  beta.start<-coef(glmnet(x, y, alpha=0, lambda=lambda, intercept=F))
  beta.mat<-matrix(NA,ncol=ncol(beta.start),nrow=nrow(beta.start)-1)
  
  # take each lambda value in the lambda
  for(i in 1:length(lambda)){
    # initial beta without intercept
    beta_prev<-as.vector(beta.start[-1,i])
    # initial converge
    converge<-10^10
    
    # iteration until converge or two many iterations
    iteration<-0
    while(converge>eta && iteration<=100){
      iteration<-iteration+1
      # judge whether some beta is small enough
      del<-which(abs(beta_prev)<eta)
      
      # if all coefficient are small enough then stop the iteration
      if(length(del)==length(beta_prev)){
        beta_prev[del]<-0
        converge<-0
        
        # else if we need to remove some but not all of the coefficients  
      }else if(length(del)>0){
        # set these beta to 0
        beta_prev[del]<-0
        
        # update design matrix x
        x.new<-x; 
        x.new<-x.new[,-del]
        
        # calculate the diagonal matrix involving penalty terms
        # and the next beta
        if(length(beta_prev)-length(del)==1){
          diag<-lambda[i]*q*(abs(beta_prev[-del])^(q-2))/2
        }else{
          diag<-diag(lambda[i]*q*(abs(beta_prev[-del])^(q-2))/2)
        }
        
        # next beta
        beta_curr<-beta_prev
        beta_curr[-del]<-solve(t(x.new)%*%x.new+diag)%*%t(x.new)%*%y
        
        # new converge value
        converge<-sum((beta_curr-beta_prev)^2)
        # next iteration
        beta_prev<-beta_curr
        
        # if we don't need to remove the coefficients
      }else{
        x.new<-x
        diag<-diag(lambda[i]*q*(abs(beta_prev)^(q-2))/2)
        # next beta
        beta_curr<-solve(t(x.new)%*%x.new+diag)%*%t(x.new)%*%y
        
        # new converge value
        converge<-sum((beta_curr-beta_prev)^2)
        # next iteration
        beta_prev<-beta_curr
        
      }
    }
    
    beta.mat[,i]<-beta_prev
  }
  colnames(beta.mat)<-lambda
  return(beta.mat)
}
