#' Compute the weights (wts)
#' 
#' This function is used to compute weights (wts)
#' 
#' @param X a matrix with counts or densities arranged in species by time step
#' 
#' @return \code{wts} returns an object of class list consisting of
#' \item{frequency}{a vector from 0 to 1 (not include 0 and 1)}
#' \item{wts}{a vector of wts}
#' 
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references <Lei paper>
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' ans<-wts(X)
#' 
#' @export
#' @importFrom stats var

wts<-function(X)
{
  cospec<-cospect(X)
  lenfreq <- length(cospec$frequency)
  cospec$frequency<-cospec$frequency[2:lenfreq]
  cospec$cospectrum<-cospec$cospectrum[,,2:lenfreq]
  
  strength<-colSums(apply(cospec$cospectrum, 3, diag))/sum(apply(X, MARGIN=1, stats::var))

  return(list(frequency=cospec$frequency, wts=strength))
}
