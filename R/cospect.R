#' Calculate the cospectrum between all pairs of time series
#' 
#' This function is used to calculate the cospectra between pairs of time series,
#' including each time series with itself. These are based on simple ffts without
#' smoothing.
#' 
#' @param X a matrix with counts or densities arranged in species by time step.
#'
#' @return \code{cospect} return a list with elements
#' \item{frequency}{a vector from 0 to 1 of the frequencies used}
#' \item{cospectrum}{a 3D array, with cospectrum range in species by species by frequency}
#'  
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' ans<-cospect(X)
#' 
#' @export 
#' @importFrom stats fft

cospect<-function(X)
{
  errcheck_data(X,"cospect")
  
  tslength<-dim(X)[2] 
  freqs<-seq(from=0,by=1/tslength,length.out=tslength)
  
  allffts<-matrix(NA,nrow(X),ncol(X))
  for (i in 1:nrow(X))
  {
    allffts[i,]<-stats::fft(X[i,])
  }
  
  cosp <- array(NA,dim=c(nrow(X), nrow(X), tslength))
  for (i in 1:nrow(X)){
    for (j in 1:nrow(X)){
      cosp[i,j,]<-(Re(Conj(allffts[i,])*allffts[j,]))/tslength/(tslength-1)
    }
  }
  
  return(list(frequency=freqs, cospectrum=cosp))
}
