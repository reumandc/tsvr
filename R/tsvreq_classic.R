#' Creator function for the \code{tsvreq_classic} S3 class
#' 
#' The \code{tsvreq_classic} (timescale-specific variance ratio equation, classic variance
#' ratio) class is for storing functional equations based on a timescale specific version of
#' the classic variance ratio. Inherits from \code{tsvreq}, which inherits from \code{list}.
#' 
#' @param X a matrix with counts or densities arranged in species by years
#' 
#' @return \code{tsvreq_classic} returns a \code{tsvreq_classic} object. Slots are:
#' \item{ts}{a vector of timescales}
#' \item{com}{a timescale-specific decomposition of CVcom2}
#' \item{comnull}{a timescale-specific decomposition of CVcomip2}
#' \item{tsvr}{a timescale-specific version of the classic variance ratio}
#' \item{wts}{a vector of weights, same length as all the above}
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @examples
#' X<-matrix(runif(10*100),10,100)
#' res<-tsvreq_classic(X)
#' 
#' @references 
#' Zhao et al, (In prep) Decomposition of the variance ratio illuminates timescale-specific
#' population and community variability.
#' 
#' @seealso \code{\link{tsvreq_classic_methods}}, \code{\link{tsvreq}}, \code{\link{vreq_classic}}, 
#' \code{browseVignettes("tsvr")}
#' 
#' @export

tsvreq_classic<-function(X)
{
  errcheck_data(X,"tsvreq_classic")
  
  #get ts and tsvr
  fsvr_classic<-vrf(X)
  freq<-fsvr_classic$frequency
  ts<-1/freq
  tsvr<-fsvr_classic$vr

  #get com
  CVcom2<-cv2f(X, type="com")
  com<-CVcom2$cv2
  
  #get comnull
  CVcomip2<-cv2f(X, type="comip")
  comnull<-CVcomip2$cv2
  
  #get wts
  wts.res<-wts(X)
  wts.res<-wts.res$wts
  
  errcheck_tsvreq(ts=ts,com=com,comnull=comnull,tsvr=tsvr,wts=wts.res)
  
  result<-list(ts=rev(ts),com=rev(com),comnull=rev(comnull),tsvr=rev(tsvr),wts=rev(wts.res))
  class(result)<-c("tsvreq_classic","tsvreq","list")
  return(result)
}