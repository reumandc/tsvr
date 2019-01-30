#' Aggregates a \code{tsvreq_classic} object across a timescale band; also the constructor function
#' for class \code{vreq_atsclassic}.
#'
#' All the components of a \code{tsvreq_classic} object can be aggregated across an arbitrary 
#' set of timescales, producing a new variance ratio equation - this function performs that
#' aggregation. The function returns a \code{vreq_atsclassic} object, and is the constructor 
#' function of that class. The letters "ats" stand for "aggregated timescales". The 
#' \code{vreq_atsclassic} class has slots \code{com}, \code{comnull}, \code{vr}, which are
#' the same as a \code{vreq} object, but also has slot \code{ts}, which is the timescales
#' over which aggregation was performed to get the object. The class inherits from \code{vreq},
#' which inherits from \code{list}.
#' 
#' @param obj A \code{tsvreq_classic} object
#' @param ts The timescales to aggregate over
#' 
#' @return \code{aggts} returns an object of class \code{vreq_atsclassic}. Slots are:
#' \item{com} the timescale aggregated value of CVcom2
#' \item{comnull} the timescale aggregated value of CVcomip2
#' \item{vr} the timescale aggregated value of the classic variance ratio
#' \item{ts} the timescales over which aggregation was performed
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@@idiv.de}; Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @export

aggts<-function(obj,ts)
{
  
  
  
  class(res)<-c("vreq_atsclassic","vreq","list")
  return(res)
}