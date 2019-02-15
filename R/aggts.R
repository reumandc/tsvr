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
#' @details Before aggregation is performed, the argument `ts` is intersected with the 
#' canonical Fourier timescales greater than or equal to the Nyquist timescale, and the
#' resulting timescales are then reflected about the Nyquist timescale. This is to 
#' account for the symmetry of Fourier transforms about the Nyquist frequency. The
#' `ts` slot of the output object shows the intersected, reflected timescales that were 
#' actually used for aggregation. See the examples.
#' 
#' @author Shaopeng Wang, \email{shaopeng.wang@@pku.edu.cn}; Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Zhao et al, (In prep) Decomposition of the variance ratio illuminates timescale-specific
#' population and community variability.
#' 
#' @seealso \code{\link{tsvreq_classic}}, \code{\link{vreq_classic_ag_methods}}, 
#' \code{browseVignettes("tsvr")}
#' 
#' @examples 
#' X<-matrix(runif(10*100),10,100)
#' h<-tsvreq_classic(X)
#' res1<-aggts(h,h$ts[h$ts>=4]) 
#' res2<-aggts(h,h$ts[h$ts>=4 | h$ts<=4/3]) 
#' #res1 and res2 produce the same result 
#' #because of Fourier symmetry around the 
#' #Nyquist timescale - see Details 
#' 
#' @export

aggts<-function(obj,ts)
{
  #make sure the class of obj is tsvreq_classic
  if (class(obj)[1]!="tsvreq_classic")
  {
    stop("Error in aggts: obj must be of class tsvreq_classic")
  }
  errcheck_tsvreq(ts=obj$ts,com=obj$com,comnull=obj$comnull,tsvr=obj$tsvr,wts=obj$wts)
  
  #prepare ts
  if (!is.numeric(ts))
  {
    stop("Error in aggts: ts must be a numeric vector")
  }
  ts<-ts[ts %in% obj$ts[obj$ts>=2]] #intesect with canonical Fourier timescales
  if (length(ts)==0)
  {
    stop("Error in aggts: elements of ts must be in obj$ts and >= 2")
  }
  T<-max(obj$ts) #length of the original time series
  if (T %% 2 == 0) #even-length time series
  {
    midind<-T/2
  }else #odd-length time series
  {
    midind<-T/2
  }
  inds<-which(obj$ts %in% ts)
  inds<-round(sort(unique(c(-inds+2*midind,inds))))
  ts<-obj$ts[inds]  
  
  #do the aggregating
  com<-sum(obj$com[inds])
  comnull<-sum(obj$comnull[inds])
  vr<-sum(obj$wts[inds]*obj$tsvr[inds])/(sum(obj$wts[inds]))

  #prep the result and return  
  res<-list(com=com,comnull=comnull,vr=vr,ts=ts)
  class(res)<-c("vreq_classic_ag","vreq","list")
  return(res)
}