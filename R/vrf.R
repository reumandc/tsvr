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
 
  #get the numerator
  totts<-apply(FUN=sum,X=X,MARGIN=2)
  h<-cospect(matrix(totts,1,length(totts)))
  numer<-h$cospectrum[1,1,2:(dim(h$cospectrum)[3])]
  freq<-h$frequency[2:length(h$frequency)]
  
  #get the denominator
  allspects<-matrix(NA,dim(X)[1],dim(X)[2]-1)
  for (counter in 1:(dim(X)[1]))
  {
    h<-cospect(X[counter,,drop=FALSE])
    allspects[counter,]<-h$cospectrum[1,1,2:(dim(h$cospectrum)[3])]
  }
  denom<-apply(FUN=sum,X=allspects,MARGIN=2)
  
  return(list(frequency=freq, vr=numer/denom))
}
