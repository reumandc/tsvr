#' Compute the weight (wts)
#' 
#' This function is used to compute weights (wts)
#' 
#' @param X a matrix with counts or densities arranged in species by time step
#' @param method If \code{"classic"}, use the classical method. If \code{"LdM"}, use the 
#' Loreau-Mazancourt method.
#' 
#' @return \code{wts} returns an object of class list consisting of
#' \item{frequency}{a vector from 0 to 1 (not include 0 and 1)}
#' \item{wts}{a vector of wts}
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-pow(X, fig=T)
#' 
#' @export

wts<-function(X, method)
{
  cospec<-cospect(X)
  lenfreq <- length(cospec$frequency)
  cospec$frequency<-cospec$frequency[2:lenfreq]
  cospec$cospectrum<-cospec$cospectrum[,,2:lenfreq]
  
  #get the denominator
  if(method=="LdM")
  {
    strength<-(colSums(sqrt(apply(cospec$cospectrum, 3, diag)))/sum(sqrt(apply(X, MARGIN=1, var))))^2
  }else
  {
    strength<-colSums(apply(cospec$cospectrum, 3, diag))/sum(apply(X, MARGIN=1, var))
  }

  return(list(frequency=cospec$frequency, wts=strength))
}
