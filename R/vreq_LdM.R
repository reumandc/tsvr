#' Creator function of \code{vreq_LdM} S3 class
#' 
#' The \code{vreq_LdM} (variance ratio equation, Loreau-de Mazancourt variance ratio) class is for 
#' storing equations based on the L-dM variance ratio. Inherits from the \code{vreq} class,
#' which inherits from the \code{list} class.
#'
#' @param X A matrix with counts or densities arranged in species by years
#' 
#' @return \code{vreq_LdM} returns a \code{vreq_LdM} object. Slots are:
#' \item{com}{the squared community CV, CVcom2}
#' \item{comnull}{CVpop2}
#' \item{vr}{the L-dM variance ratio}
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#'
#' @references 
#' Loreau & Mazancourt, Species Synchrony and Its Drivers: Neutral and Nonneutral Community 
#' Dynamics in Fluctuating Environments. 2008, Am. Nat. 172(2)
#' 
#' @seealso \code{\link{vreq_LdM_methods}}, \code{\link{vreq_classic}}, \code{\link{vreq}},
#' \code{browseVignettes("tsvr")}
#' 
#' @examples
#' X<-matrix(runif(10*100),10,100)
#' res<-vreq_LdM(X)
#' 
#' @export

vreq_LdM<-function(X)
{
  errcheck_data(X,"vreq_LdM")
  
  vr_LdM<-vr(X, method="LdM")
  CVcom2<-cv2(X, type="com")
  CVpop2<-cv2(X, type="pop")
  
  errcheck_vreq(com=CVcom2,comnull=CVpop2,vr=vr_LdM)
  
  result<-list(com=CVcom2,comnull=CVpop2,vr=vr_LdM)
  class(result)<-c("vreq_LdM","vreq","list")
  return(result)
}
