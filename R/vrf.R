#' Compute the frequency-specific variance ratio or the frequency decomposition of the 
#' variance ratio
#' 
#' This function computes the frequency-specific variance ratio or the frequency-
#' decomposition of the variance ratio for a community in a single plot. It is similar 
#' to \code{\link{vr}}, but uses the cospectrum instead of the covariance.
#' 
#' @param X a matrix with counts or densities arranged in species by time step
#' @param method If \code{classic}, uses the classical variance ratio. If \code{LdM}, uses 
#' the Loreau-Mazancourt method.
#' @param flag If \code{fd}, gives the frequency decomposition of the variance ratio, 
#' where only numerator is made timescale specific. If \code{fs},  gives the frequency-specific 
#' variance ratio, where both numerator and denominator are made frequency specific.
#' 
#' @return \code{vrf} returns a list consisting of
#' \item{frequency}{a vector from 0 to 1 (not including 0 and 1)}
#' \item{vr}{a vector of frequency-specific or frequency-decomposition of VR}
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-vrf(X, fig=T)
#' 
#' @export

vrf<-function(X, method, flag){
  
  #Compute all the cospectrum and arrange them into
  #a 3D array, species by species by frequency 
  cospec<-cospect(X)
  lenfreq <- length(cospec$frequency)
  cospec$frequency<-cospec$frequency[2:lenfreq]
  cospec$cospectrum<-cospec$cospectrum[,,2:lenfreq]
  
  vr.f <- apply(X=cospec$cospectrum, MARGIN=3, FUN=sum,na.rm=T)
  
  #get the denominator
  if(method=="LdM"){
    if(flag=="fs")
    {
      D<-(colSums(sqrt(apply(cospec$cospectrum, 3, diag))))^2
    }else
    {
      D<-(sum(sqrt(apply(X, MARGIN=1, var))))^2
    }
  }else
  {
    if(flag=="fs")
    {
      D<-colSums(apply(cospec$cospectrum, 3, diag))
    }else
    {
      D<-sum(apply(X, MARGIN=1, var))
    }
  }
  
  vr.f <- vr.f/D
  #vr.f[D<1e-10]<-0    # should I add this? ***Dan does not see what for, taking it out for now
  
  return(list(frequency=cospec$frequency, vr=vr.f))
}
