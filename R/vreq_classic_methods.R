#' Basic methods for the \code{vreq_classic} class
#' 
#' Set, get, summary, and print methods for the \code{vreq_classic} class.
#' 
#' @param object,x,obj An object of class \code{vreq_classic}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.vreq_classic} produces a summary of a \code{vreq_classic} object.
#' A \code{print.vreq_classic} method is also available. For \code{vreq_classic} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{vreq_classic} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{vreq_classic} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Peterson (1975) Stability of species and of community for the benthos of two lagoons. Ecology 56, 958-965.
#' 
#' @seealso \code{\link{vreq_classic}}
#' 
#' @examples
#' X<-matrix(runif(10*100),10,100)
#' res<-vreq_classic(X)
#' print(res)
#' summary(res)
#'  
#' @name vreq_classic_methods
NULL
#> NULL

#' @rdname vreq_classic_methods
#' @export
summary.vreq_classic<-function(object,...)
{
  res<-list(class="vreq_classic",
            com=get_com(object),
            comnull=get_comnull(object),
            vr=get_vr(object))
  
  #a summary_tsvr object inherits from the list class, but has its own print method
  class(res)<-c("summary_tsvr","list")
  return(res)
}

#' @rdname vreq_classic_methods
#' @export
print.vreq_classic<-function(x,...)
{
  cat(paste0("Object of class vreq_classic:\n CVcom2: ",get_com(x),"\n CVcomip2: ",get_comnull(x),"\n classic vr: ",get_vr(x)))
}

#' @rdname vreq_classic_methods
#' @export
set_com.vreq_classic<-function(obj,newval)
{
  stop("Error in set_com: vreq_classic slots should not be changed individually")
}

#' @rdname vreq_classic_methods
#' @export
set_comnull.vreq_classic<-function(obj,newval)
{
  stop("Error in set_comnull: vreq_classic slots should not be changed individually")
}

#' @rdname vreq_classic_methods
#' @export
set_vr.vreq_classic<-function(obj,newval)
{
  stop("Error in set_vr: vreq_classic slots should not be changed individually")
}

#' @rdname vreq_classic_methods
#' @export
get_com.vreq_classic<-function(obj)
{
  return(obj$com)
}

#' @rdname vreq_classic_methods
#' @export
get_comnull.vreq_classic<-function(obj)
{
  return(obj$comnull)
}

#' @rdname vreq_classic_methods
#' @export
get_vr.vreq_classic<-function(obj)
{
  return(obj$vr)
}
