#' Calculate the variability partitioning for CVpop, CVcom, CVcomip
#' 
#' This function is used to calculate the variability partitioning.
#' 
#' @param X a matrix with counts or densities arranged in species by time step.
#' @param type If \code{pop}, calculate a frequency-specific CVpop2. If \code{com}, 
#' calculate a frequency-specific CVcom2. If \code{comip}, calculate a frequency-
#' specific CVcomip2. 
#'
#' @return \code{cv2f} return an object of type list consisting of
#' \item{frequency}{a vector from 0 to 1 (not including 0 and 1)}
#' \item{cv2}{a vector of frequency-specific population or community variability}
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@idiv.de}; Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-cv2f(X, type="com", fig=T)
#' 
#' @export

cv2f <- function(X, type){
  errcheck_data(X)

  meanSp <- rowMeans(X)
  cosp<-cospect(X)
  freq<-cosp[[1]][-1]
  covSp <- cosp[[2]][,,-1]  # cospectrum
  mean.comm <- sum(meanSp)
  
  if (type=="com")
  {     
    var.comm <- apply(covSp, 3, sum)
    cv2 <- var.comm/mean.comm^2
    return(list(frequency=freq,cv2=cv2))
  }
  if (type=="comip")
  {
    cv2 <- apply(covSp, 3, function(x){
      var.sp <- sum(diag(x))
      cv2 <- var.sp/mean.comm^2   
      return(cv2)
    })
    return(list(frequency=freq,cv2=cv2))
  }
  if ("pop")
  {
    #***DAN: do this
    stop("Error in cv2f: type='pop' option not implemented yet")
  }
  stop("Error in cv2f: type must be com, comip, or pop")
}
