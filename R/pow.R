#' Compute the power of oscillation
#' 
#' This function is used to compute the power of oscillation
#' 
#' @param X a matrix with counts or densities arranged in species (or other scale) by years
#' @param method If \code{"classic"} (default), use the classical method. 
#' If \code{"LdM"}, use the Loreau-Mazancourt method (see reference).
#' @param fig logical. If \code{FALSE} (default), do not output the figure. If \code{TRUE}, 
#' plot the figure with x-axis frequency and y-axis power of oscillation
#' 
#' @return \code{pow} return an object of list consisting of
#' \item{frequency}{a vector from 0 to 1 (not include 0 and 1)}
#' \item{strength}{a vector of power of oscillation}
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references (Our draft)
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-pow(X, fig=T)
#' 
#' @export

pow<-function(X, method="classic", fig=FALSE){
  

  cospec<-cospect(X)
  lenfreq <- length(cospec$frequency)
  cospec$frequency<-cospec$frequency[2:lenfreq]
  cospec$cospectrum<-cospec$cospectrum[,,2:lenfreq]
  
  #get the denominator
  if(method=="LdM"){
    strength<-(colSums(sqrt(apply(cospec$cospectrum, 3, diag)))/sum(sqrt(apply(X, MARGIN=1, var))))^2
  }else{
    strength<-colSums(apply(cospec$cospectrum, 3, diag))/sum(apply(X, MARGIN=1, var))
  }
  
  if(fig){
    plot(cospec$frequency, strength, xlab="frequency (cycles/year)", ylab="strength of oscillation",
           xlim=c(0,1), ylim=c(0, max(strength)), typ="l")
    text(0.5,max(strength),labels=paste("pow=",as.character(round(sum(strength),4)),sep=''),adj=c(1.05,-.4))
    lines(c(.5,.5),c(0,max(strength)),lty='dotted')
    rect(.5,-max(strength),2,2*max(strength),density=NA,col=rgb(0,0,0,alpha=0.2))
  }
  
  return(list(frequency=cospec$frequency, strength=strength))
}
