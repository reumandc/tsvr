#' Creator function of \code{vreq_classic} S3 class
#' 
#' The \code{vreq_classic} (variance ratio equation, classic variance ratio) class is for 
#' storing equations based on the classic variance ratio. Inherits from the \code{vreq} class,
#' which inherits from the \code{list} class.
#'
#' @param X A matrix with counts or densities arranged in species by years
#' 
#' @return \code{vreq_classic} returns a \code{vreq_classic} object. Slots are:
#' \item{com} the squared community CV, CVcom2
#' \item{comnull} CVcomip2
#' \item{vr} the classic variance ratio
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @examples
#' #add later
#' 
#' @export

vreq_classic<-function(X)
{
  vr_classic<-vr(X, method="classic")
  CVcom2<-cv2(X, type="com")
  CVcomip2<-cv2(X, type="comip")
  
  errcheck_vreq(com=CVcom2,comnull=CVcomip2,vr=vr_classic)
      
  result<-list(com=CVcom2,comnull=CVcomip2,vr=vr_classic)
  class(result)<-c("vreq_classic","vreq","list")
  return(result)
}

