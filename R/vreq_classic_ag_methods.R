#' Basic methods for the \code{vreq_classic_ag} class
#' 
#' Set, get, summary, and print methods for the \code{vreq_classic_ag} class.
#' 
#' @param object,x,obj An object of class \code{vreq_classic_ag}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.vreq_classic_ag} produces a summary of a \code{vreq_classic_ag} object.
#' A \code{print.vreq_classic_ag} method is also available. For \code{vreq_classic_ag} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{aggts} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{vreq_classic_ag} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Zhao et al, (In prep) Decomposition of the variance ratio illuminates timescale-specific
#' population and community variability.
#' 
#' @seealso \code{\link{aggts}}, \code{\link{tsvreq_classic}}, 
#' \code{\link{vreq_classic}}, \code{\link{vreq_LdM}}, \code{\link{vreq}}, 
#' \code{browseVignettes("tsvr")}
#' 
#' @examples
#' X<-matrix(runif(10*100),10,100)
#' h<-tsvreq_classic(X)
#' inp<-aggts(h,h$ts[h$ts>4])
#' print(inp)
#' summary(inp)
#'  
#' @name vreq_classic_ag_methods
NULL
#> NULL

#' @rdname vreq_classic_ag_methods
#' @export
summary.vreq_classic_ag<-function(object,...)
{
  res<-list(class="vreq_classic_ag",
            com=get_com(object),
            comnull=get_comnull(object),
            vr=get_vr(object),
            ts_start=object$ts[1],
            ts_end=object$ts[length(object$ts)],
            ts_length=length(object$ts))
  
  #a summary_tsvr object inherits from the list class, but has its own print method
  class(res)<-c("summary_tsvr","list")
  return(res)
}

#' @rdname vreq_classic_ag_methods
#' @export
print.vreq_classic_ag<-function(x,...)
{
  cat(paste0("Object of class vreq_classic_ag:\n CVcom2: ",get_com(x),"\n CVcomip2: ",get_comnull(x),"\n vr: ",get_vr(x),"\n ts: "))
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$ts,3)),"\n")  
  }else
  {
    cat(paste(signif(x$ts[1:3],3)),"...",paste(signif(x$ts[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
}

#' @rdname vreq_classic_ag_methods
#' @export
set_com.vreq_classic_ag<-function(obj,newval)
{
  stop("Error in set_com: vreq_classic_ag slots should not be changed individually")
}

#' @rdname vreq_classic_ag_methods
#' @export
set_comnull.vreq_classic_ag<-function(obj,newval)
{
  stop("Error in set_comnull: vreq_classic_ag slots should not be changed individually")
}

#' @rdname vreq_classic_ag_methods
#' @export
set_vr.vreq_classic_ag<-function(obj,newval)
{
  stop("Error in set_vr: vreq_classic_ag slots should not be changed individually")
}

#' @rdname vreq_classic_ag_methods
#' @export
set_ts.vreq_classic_ag<-function(obj,newval)
{
  stop("Error in set_ts: vreq_classic_ag slots should not be changed individually")
}

#' @rdname vreq_classic_ag_methods
#' @export
get_com.vreq_classic_ag<-function(obj)
{
  return(obj$com)
}

#' @rdname vreq_classic_ag_methods
#' @export
get_comnull.vreq_classic_ag<-function(obj)
{
  return(obj$comnull)
}

#' @rdname vreq_classic_ag_methods
#' @export
get_vr.vreq_classic_ag<-function(obj)
{
  return(obj$vr)
}

#' @rdname vreq_classic_ag_methods
#' @export
get_ts.vreq_classic_ag<-function(obj)
{
  return(obj$ts)
}

