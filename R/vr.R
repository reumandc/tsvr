#' Compute the classic or Loreau-Mazancourt variance ratio  
#' 
#' This function is used to compute the classical/Loreau-Mazancourt variance ratio for a community in a single plot or for 
#' a single species in multiple plots.
#' 
#' @param X a matrix with counts or densities arranged in species (or other scale) by years
#' @param method If \code{"classic"} (default), use the classical method. If \code{"LdM"}, use the Loreau-Mazancourt method (see reference).
#' 
#' @return \code{vr} return the value of variance ratio
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Loreau & Mazancourt, Species Synchrony and Its Drivers: Neutral and Nonneutral Community Dynamics in Fluctuating Environments. 2008, Am. Nat. 172(2)
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' vr(X, method="LdM")
#' vr(X, method="classic")
#' 
#' @export

vr<-function(X, method="classic")
{
  #Compute all the variances and covariances and arrange them into
  #a 2D array. At the end of these lines, covs(i1,i2)
  #should have the covariance of xi1 and xi2, using notation
  #from the document.
  covs<-cov(t(X))
  
  #get the denominator
  if(method=="LdM"){
    D <- (sum(sqrt(diag(covs))))^2
  }else{D <- sum(diag(covs))}
  
  #term 2 in the doc
  total<-sum(covs)
  
  #term 3 in the doc
  var.ratio<-total/D
  
  return(var.ratio)
  
}