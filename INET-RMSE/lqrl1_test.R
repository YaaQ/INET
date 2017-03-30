#######################################################################
### All rights are reserved by the authors.
### Authors: Weining Wang, Lining Yu, Humboldt-Universität zu Berlin (yulining@hu-berlin.de)
#######################################################################
linear      = function(Qy, Qxx, Qp, Qi, Qj) {
  fit       = qrL1(Qxx, Qy, Qp, 50)
  isnum     = which(fit$Cgacv == min(fit$Cgacv))
  beta.in   = fit$beta[isnum, ]
  lambda_in = (-fit$lambda[isnum])
  print(lambda_in)
  beta0     = fit$beta0[isnum]
  print(which(beta.in != 0))
  beta.new  = c(beta0, beta.in)
  #beta.new  = beta.new/sqrt(sum(beta.new^2))
  #Lest      = c(1, Qxx[ncol(Qxx),])
  #pre_est   = Lest %*% beta.new
  
  finalresults = list()
  finalresults$lambda.in = lambda_in
  finalresults$beta_in   = beta.new
  #finalresults$pre_est   = pre_est
  return(finalresults)
}


