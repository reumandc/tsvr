#' Calculate the cospectrum between all pairs (including pairs with itself)
#' 
#' This function is used to calculate the cospectrum.
#' 
#' @param X a matrix with counts or densities arranged in species (or other scale) by years.
#' Notice the step of years should be exactly the same.
#'
#' @return \code{cospect} return a list with elements
#' \item{frequency}{a vector from 0 to 0.5 (or a little less) of the frequencies used}
#' \item{cospectrum}{a 3D array, with cospectrum range in species by species by frequency}
#'  
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
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
  #***DAN: can this function handle vector input? (the one-species case, useful, e.g., for passing the total timeseries to get the cospectrum of that)
  tmp <- X[1,]
  tslength<-length(tmp) #***DAN: not best way to get the length
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
