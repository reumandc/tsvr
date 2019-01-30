#' Calculate the cospectrum between all pairs (including pairs with itself)
#' 
#' This function is used to calculate the cospectrum.
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
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' ans<-cospect(X)
#' 
#' @keywords internal

cospect<-function(X)
{
  errcheck_data(X,"cospect")
  
  tslength<-dim(X)[2] 
  freqlen<-seq(from=0,by=1/tslength,length.out=tslength)
  
  cosp <- array(NA,dim=c(nrow(X), nrow(X), length(freqlen)))
  for (i in 1:nrow(X)){
    a <- X[i,]
    ffta<-fft(a)
    for (j in 1:nrow(X)){
      b <- X[j,]
      fftb<-fft(b)
      cospectrum<-(Re(Conj(ffta)*fftb))/tslength/(tslength-1)
      
      cosp[i,j,]<-cospectrum[1:length(freqlen)]
    }
  }
  return(list(frequency=freqlen, cospectrum=cosp))
}
