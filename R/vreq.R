#' Creator function for the \code{vreq} S3 class
#' 
#' The \code{vreq} (variance ratio equation) class is for storing equations based on a
#' variance ratio, as in Wang S. & Loreau M. (2016). Biodiversity and ecosystem stability 
#' across scales in metacommunities. ECOL LETT, 19, 510-518. This is a general class 
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
#' \item{values}{a single positive number}
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@@idiv.de}; Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Wang S. & Loreau M. (2016). Biodiversity and ecosystem stability across scales in metacommunities. ECOL LETT, 19, 510-518.
#' 
#' @export

vreq<-function(com,comnull,vr)
{
  errcheck_vreq(com,comnull,vr)  
  res<-list(com=com,comnull=comnull,vr=vr)  
  class(res)<-append(class(res),"vreq") 
  return(res)
}
