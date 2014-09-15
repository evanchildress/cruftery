pow <- function(x,y) x^y

sq_distance <- function(x, y) {
  return(pow(x-y,2));
}
  
sq_circ_distance <- function(x, y) {
    if (x > y) {
      d <- min(x-y, (2.0*pi-x)+y);
    } else {
      d <- min(y-x, (2.0*pi-y)+x);
    }
    return(pow(d,2))
}
  
gp_generalized_sq_exp <- function(theta_eta_sq,  theta_rho_sq,  theta_sigma_sq,  x) {
    theta_Sigma <- matrix(data=0, nrow=length(x), ncol=length(x)) 
    for (i in 1:length(x)) {
      for (j in i:length(x)) {
        theta_Sigma[i,j] <- theta_eta_sq * exp(-theta_rho_sq * sq_distance(x[i],x[j]));
        theta_Sigma[j,i] <- theta_Sigma[i,j];
      }
    }
    
    for (k in 1:length(x)) {
      theta_Sigma[k,k] <- theta_eta_sq + theta_sigma_sq;
    }
    
    return(theta_Sigma)
  }
  
gp_circ_generalized_sq_exp <- function(theta_eta_sq, theta_rho_sq, theta_sigma_sq, x) {
  theta_Sigma <- matrix(data=0, nrow=length(x), ncol=length(x)) 
  for (i in 1:length(x)) {
    for (j in i:length(x)) {
      theta_Sigma[i,j] <- theta_eta_sq * exp(-theta_rho_sq * sq_circ_distance(x[i],x[j]));
    	theta_Sigma[j,i] <- theta_Sigma[i,j];
  	}
  }
    
  for (k in 1:length(x)) {
  	theta_Sigma[k,k] <- theta_eta_sq + theta_sigma_sq;
  }
    
	return(theta_Sigma);
}

#angle <- (yday(time)/366-0.5)*2*pi
#
#theta_eta_sq <- 10
#theta_rho_sq <- 1
#theta_sigma_sq <- 1
#theta_Sigma <- gp_circ_generalized_sq_exp(theta_eta_sq, theta_rho_sq, theta_sigma_sq, angle)
#positions <- mvrnorm(n=1, mu=mu, Sigma=theta_Sigma)
#qplot(angle, positions)

