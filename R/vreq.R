#' Creator function for the \code{vreq} S3 class
#' 
#' The \code{vreq} (variance ratio equation) class is for storing equations based on a
#' variance ratio, as in Wang S. & Loreau M. (2016) Biodiversity and ecosystem stability 
#' across scales in metacommunities, Ecol Lett, 19, 510-518. This is a general class 
#' from which other classes inherit (\code{vreq_classic}, \code{vreq_LdM}). \code{vreq} 
#' inherits from the \code{list} class. 
#' 
#' @param com A single positive number
#' @param comnull Another single positive number
#' @param vr Another single positive number
#' 
#' @return \code{vreq} returns an object of class \code{vreq}. Slots are: 
#' \item{com}{a single positive number equal to \code{comnull}*\code{vr}} 
#' \item{comnull}{a single positive number}
#' \item{vr}{a single positive number}
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@@pku.edu.cn}; Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Wang S. & Loreau M. (2016) Biodiversity and ecosystem stability across scales in metacommunities. Ecol Lett, 19, 510-518.
#' 
#' Zhao et al, (In prep) Decomposition of the variance ratio illuminates timescale-specific
#' population and community variability.
#' 
#' Peterson, Stability of species and of community for the benthos of two lagoons. 1975,
#' Ecology, 56, 958-965.
#' 
#' @seealso \code{\link{vreq_methods}}, \code{\link{vreq_classic}}, \code{\link{vreq_LdM}}, 
#' \code{\link{vreq_classic_ag_methods}}, \code{browseVignettes("tsvr")} 
#' 
#' @example
#' res<-vreq(com=2,comnull=1,vr=2)
#'   
#' @export

vreq<-function(com,comnull,vr)
{
  errcheck_vreq(com,comnull,vr)  
  res<-list(com=com,comnull=comnull,vr=vr)  
  class(res)<-c("vreq","list") 
  return(res)
}
