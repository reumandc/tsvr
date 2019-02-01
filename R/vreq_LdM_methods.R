#' Basic methods for the \code{vreq_LdM} class
#' 
#' Set, get, summary, and print methods for the \code{vreq_LdM} class.
#' 
#' @param object,x,obj An object of class \code{vreq_LdM}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.vreq_LdM} produces a summary of a \code{vreq_LdM} object.
#' A \code{print.vreq_LdM} method is also available. For \code{vreq_LdM} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{vreq_LdM} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{vreq_LdM} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references Loreau & Mazancourt, Species Synchrony and Its Drivers: Neutral and Nonneutral Community 
#' Dynamics in Fluctuating Environments. 2008, Am. Nat. 172(2)
#' 
#' @seealso \code{\link{vreq_LdM}}
#' 
#' @examples
#' X<-matrix(runif(10*100),10,100)
#' res<-vreq_LdM(X)
#' print(res)
#' summary(res)
#'  
#' @name vreq_LdM_methods
NULL
#> NULL

#' @rdname vreq_LdM_methods
#' @export
summary.vreq_LdM<-function(object,...)
{
  res<-list(class="vreq_LdM",
            com=get_com(object),
            comnull=get_comnull(object),
            vr=get_vr(object))
  
  #a summary_tsvr object inherits from the list class, but has its own print method
  class(res)<-c("summary_tsvr","list")
  return(res)
}

#' @rdname vreq_LdM_methods
#' @export
print.vreq_LdM<-function(x,...)
{
  cat(paste0("Object of class vreq_LdM:\n CVcom2: ",get_com(x),"\n CVpop2: ",get_comnull(x),"\n LdM vr: ",get_vr(x)))
}

#' @rdname vreq_LdM_methods
#' @export
set_com.vreq_LdM<-function(obj,newval)
{
  stop("Error in set_com: vreq_LdM slots should not be changed individually")
}

#' @rdname vreq_LdM_methods
#' @export
set_comnull.vreq_LdM<-function(obj,newval)
{
  stop("Error in set_comnull: vreq_LdM slots should not be changed individually")
}

#' @rdname vreq_LdM_methods
#' @export
set_vr.vreq_LdM<-function(obj,newval)
{
  stop("Error in set_vr: vreq_LdM slots should not be changed individually")
}

#' @rdname vreq_LdM_methods
#' @export
get_com.vreq_LdM<-function(obj)
{
  return(obj$com)
}

#' @rdname vreq_LdM_methods
#' @export
get_comnull.vreq_LdM<-function(obj)
{
  return(obj$comnull)
}

#' @rdname vreq_LdM_methods
#' @export
get_vr.vreq_LdM<-function(obj)
{
  return(obj$vr)
}
