#' Compute the frequency-specific variance ratio 
#' 
#' This function computes the frequency-specific variance ratio for a community in a single plot. 
#' 
#' @param X a matrix with counts or densities arranged in species by time step
#' 
#' @return \code{vrf} returns a list consisting of
#' \item{frequency}{a vector from 0 to 1 (not including 0 and 1)}
#' \item{vr}{a vector of frequency-specific or frequency-decomposition of VR}
#' 
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references <Lei's paper>
#' 
#' @seealso \code{\link{vr}}
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' ans<-vrf(X)
#' 
#' @export

vrf<-function(X){
  
  errcheck_data(X,"vrf")
  
  #Compute all the cospectrum and arrange them into
  #a 3D array, species by species by frequency 
  cospec<-cospect(X)
  lenfreq <- length(cospec$frequency)
  cospec$frequency<-cospec$frequency[2:lenfreq]
  cospec$cospectrum<-cospec$cospectrum[,,2:lenfreq]
  
  vr.f <- apply(X=cospec$cospectrum, MARGIN=3, FUN=sum,na.rm=T)
  D<-colSums(apply(cospec$cospectrum, 3, diag))
  vr.f <- vr.f/D

  return(list(frequency=cospec$frequency, vr=vr.f))
}
