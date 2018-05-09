#' Calculate the variability partitioning 
#' 
#' This function is used to calculate the variability partitioning.
#' 
#' @param X a matrix with counts or densities arranged in species (or other scale) by years.
#' @param type If \code{"pop"} (default), calculate population CV^2. If \code{"com"}, calculate community CV^2.
#' @param fig logical. If \code{FALSE} (default), do not output the figure. If \code{TRUE}, 
#' plot the figure with x-axis frequency and y-axis CV^2.
#'
#' @return \code{cv2f} return an object of list consisting of
#' \item{frequency}{a vector from 0 to 1 (not include 0 and 1)}
#' \item{cv2.f}{a vector of frequency-specific population or community variability}
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@idiv.de}; Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' @references Wang S. & Loreau M. (2016). Biodiversity and ecosystem stability across scales in metacommunities. ECOL LETT, 19, 510-518.
#' ***DAN: why is this the reference? we don't use their measure, and they don't use a freq-specific approach
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-cv2f(X, type="com", fig=T)
#' 
#' @export

cv2f <- function(X, type="pop", fig=FALSE){
  
  X<-X[rowSums(X)>0, ]  # delete species whose counts are zeros across all the years
  #***DAN: what about NAs?
  meanSp <- rowMeans(X)
  cosp<-cospect(X)
  freq<-cosp[[1]][-1]
  covSp <- cosp[[2]][,,-1]  # cospectrum
  #***DAN: need comment on why we throw away the first one
  #***DAN: need unit tests which test that all appropriate sums of fs quantities addup to the classic quantities 
  mean.comm <- sum(meanSp)
  
  if(type=="com")
  {     # Gamma variability: frequency dependent 
    var.comm <- apply(covSp, 3, sum)
    #***DAN: wwould be more efficient to jst calculate the sum and then the cospect of that in this case
    cv2.f <- var.comm/mean.comm^2
  }else
  {               # Alpha variability: frequency dependent
    cv2.f <- apply(covSp, 3, function(x){
      var.sp <- sum(diag(x))
      cv.pop <- var.sp/mean.comm^2   ##<<<<<------- notice this defination is new
      return(cv.pop)
    })
  }
  
  if(fig){
    if(type=="com"){
      plot(freq, cv2.f, xlab="frequency (cycles/year)", ylab="",
           xlim=c(0,1), ylim=c(0, max(cv2.f)), typ="l")
      mtext(expression(paste(CV[com]^2,(italic(f)))), side=2, line=2)
      text(0.5,max(cv2.f),labels=bquote(paste(CV[com]^2,"=",.(round(sum(cv2.f),4)))),adj=c(1.05,0.4))
    }else{plot(freq, cv2.f, xlab="frequency (cycles/year)", ylab="",
               xlim=c(0,1), ylim=c(0, max(cv2.f)), typ="l")
      mtext(expression(paste(CV[pop]^2,(italic(f)))), side=2, line=2)
      text(0.5,max(cv2.f),labels=bquote(paste(CV[pop]^2,"=",.(round(sum(cv2.f),4)))),adj=c(1.05,0.4))}
    
    lines(c(.5,.5),c(0,max(cv2.f)),lty='dotted')
    rect(.5,-max(cv2.f),2,2*max(cv2.f),density=NA,col=rgb(0,0,0,alpha=0.2))
  }
  
  
  return(list(frequency=freq, cv2.f=cv2.f))
}
