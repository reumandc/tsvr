#' Creator function for the \code{tsvreq} S3 class
#' 
#' The \code{tsvreq} (timescale-specific variance ratio equation) class is for storing 
#' functional equations based on a timescale-specific variance ratio. This is a general class 
#' from which other classes inherit (only \code{tsvreq_classic} at this point). \code{tsvreq} 
#' inherits from the \code{list} class. 
#' 
#' @param ts A vector of timescales
#' @param com A numeric vector of the same length as \code{ts} containing nonnegative quantities
#' @param comnull Another such
#' @param tsvr Another such
#' @param wts Another such
#'  
#' @return \code{tsvreq} returns an object of class \code{tsvreq}. Slots are: 
#' \item{ts}{the input}
#' \item{com}{the input, equal to \code{comnull}*\code{tsvr}} 
#' \item{comnull}{the input}
#' \item{tsvr}{the input}
#' \item{wts}{the input}
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@@idiv.de}; Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @export

tsvreq<-function(ts,com,comnull,tsvr,wts)
{
  errcheck_tsvreq(ts,com,comnull,tsvr,wts)  
  res<-list(ts=ts,com=com,comnull=comnull,tsvr=tsvr,wts=wts)  
  class(res)<-c("tsvreq","list") 
  return(res)
}


