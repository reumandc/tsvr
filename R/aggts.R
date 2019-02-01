#' Aggregates a \code{tsvreq_classic} object across a set of timescales; also the constructor function
#' for class \code{vreq_classic_ag}.
#'
#' All the components of a \code{tsvreq_classic} object can be aggregated across an arbitrary 
#' set of timescales, producing a new variance ratio equation - this function performs that
#' aggregation. The function returns a \code{vreq_classic_ag} object, and is the constructor 
#' function of that class. The 
#' \code{vreq_classic_ag} class has slots \code{com}, \code{comnull}, \code{vr}, which are
#' the same as a \code{vreq} object, but also has slot \code{ts}, which is the timescales
#' over which aggregation was performed to get the object. The class inherits from \code{vreq},
#' which inherits from \code{list}.
#' 
#' @param obj A \code{tsvreq_classic} object
#' @param ts The timescales to aggregate over
#' 
#' @return \code{aggts} returns an object of class \code{vreq_classic_ag}. Slots are:
#' \item{com}{the timescale-aggregated value of CVcom2}
#' \item{comnull}{the timescale-aggregated value of CVcomip2}
#' \item{vr}{the timescale-aggregated value of the classic variance ratio}
#' \item{ts}{the timescales over which aggregation was performed}
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@@pku.edu.cn}; Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' <add Lei's paper>
#' 
#' @seealso \code{\link{tsvreq_classic}}, \code{\link{vreq_classic_ag}}, \code{browseVignettes("tsvr")}
#' 
#' @examples 
#' X<-matrix(runif(10*100),10,100)
#' h<-tsvreq_classic(X)
#' res<-aggts(h,h$ts[h$ts>4])
#' 
#' @export

aggts<-function(obj,ts)
{
  #make sure the class of obj is tsvreq_classic
  if (class(obj)[1]!="tsvreq_classic")
  {
    stop("Error in aggts: obj must be of class tsvreq_classic")
  }
  
  #make sure ts is a subset of timescales in obj$ts
  if (!all(ts %in% obj$ts))
  {
    stop("Error in aggts: elements of ts must be in obj$ts")
  }
  
  #do the aggregating
  inds<-which(obj$ts %in% ts)
  com<-sum(obj$com[inds])
  comnull<-sum(obj$comnull[inds])
  vr<-sum(obj$wts[inds]*obj$tsvr[inds])/(sum(obj$wts[inds]))

  #prep the result and return  
  res<-list(com=com,comnull=comnull,vr=vr,ts=ts)
  class(res)<-c("vreq_classic_ag","vreq","list")
  return(res)
}