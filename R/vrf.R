#' Compute the frequency-specific or frequency-decomposition version of the variance ratio
#' 
#' This function is used to compute the frequency-specific or frequency-decomposition version of variance ratio
#' for a community in a single plot or for a single species 
#' in multiple plots. It is similar to \code{\link{vr}}, but use cospectrum instead of covariance.
#' 
#' @param X a matrix with counts or densities arranged in species (or other scale) by years
#' @param method If \code{"classic"} (default), use the classical method. 
#' If \code{"LdM"}, use the Loreau-Mazancourt method (see reference).
#' @param f.flag If \code{"fd"} (default), frequency-decomposition of variance ratio, where only numerator is frequency specific.
#' If \code{"fs"},  frequency-specific variance ratio, where both numerator and denominator are frequency specific.
#' @param fig logical. If \code{FALSE} (default), do not output the figure. If \code{TRUE}, 
#' plot the figure with x-axis frequency and y-axis variance ratio.
#' 
#' @return \code{vrf} return an object of list consisting of
#' \item{frequency}{a vector from 0 to 1 (not include 0 and 1)}
#' \item{vr.f}{a vector of frequency-specific or frequency-decomposition of VR}
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Loreau & Mazancourt, Species Synchrony and Its Drivers: Neutral and Nonneutral Community Dynamics in Fluctuating Environments. 2008, Am. Nat. 172(2)
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-vrf(X, fig=T)
#' 
#' @export

vrf<-function(X, method="classic", f.flag="fd", fig=FALSE){
  
  #Compute all the cospectrum and arrange them into
  #a 3D array, species by species by frequency 
  cospec<-cospect(X)
  lenfreq <- length(cospec$frequency)
  cospec$frequency<-cospec$frequency[2:lenfreq]
  cospec$cospectrum<-cospec$cospectrum[,,2:lenfreq]
  
  vr.f <- apply(cospec$cospectrum, MARGIN=c(3), sum,na.rm=T)
  
  #get the denominator
  if(method=="LdM"){
    if(f.flag=="fs"){
      D<-(colSums(sqrt(apply(cospec$cospectrum, 3, diag))))^2
    }else{D<-(sum(sqrt(apply(X, MARGIN=1, var))))^2}
  }else{
    if(f.flag=="fs"){
      D<-colSums(apply(cospec$cospectrum, 3, diag))
    }else{D<-sum(apply(X, MARGIN=1, var))}
  }
  
  vr.f <- vr.f/D
  vr.f[D<1e-10]<-0    # should I add this?
  
  if(fig){
    if(f.flag=="fs"){
      plot(cospec$frequency, vr.f, xlab="frequency (cycles/year)", ylab="frequency specific VR",
           xlim=c(0,1), ylim=c(0, max(vr.f)), typ="l")
    }else{plot(cospec$frequency, vr.f, xlab="frequency (cycles/year)", ylab="frequency decomposition of VR",
               xlim=c(0,1), ylim=c(0, max(vr.f)), typ="l")
      text(0.5,max(vr.f),labels=paste("VR=",as.character(round(sum(vr.f),4)),sep=''),adj=c(1.05,-.4))}
    lines(c(.5,.5),c(0,max(vr.f)),lty='dotted')
    rect(.5,-max(vr.f),2,2*max(vr.f),density=NA,col=rgb(0,0,0,alpha=0.2))
  }
  
  return(list(frequency=cospec$frequency, vr.f=vr.f))
}
