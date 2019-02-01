#' Basic methods for the \code{vreq} class
#' 
#' Set, get, summary, and print methods for the \code{vreq} class.
#' 
#' @param object,x,obj An object of class \code{vreq}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.vreq} produces a summary of a \code{vreq} object.
#' A \code{print.vreq} method is also available. For \code{vreq} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{vreq} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{vreq} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{vreq}}
#' 
#' @examples
#' res<-vreq(com=2,comnull=1,vr=2)
#' print(res)
#'  
#' @name vreq_methods
NULL
#> NULL

#' Set and get methods for classes in the \code{tsvr} package
#' 
#' Set and get methods for classes in the \code{tsvr} package. There
#' are methods for each slot of each class, named \code{set_*} and
#' \code{get_*} for \code{*} the slot name. Below are listed function
#' specs for the generics and the default methods.
#' 
#' @param obj An object of one of the classes defined in the package
#' @param newval A newvalue of the slot in question, for the \code{set_*} methods
#' 
#' @return \code{set_*} methods throw an error - setting of individual
#' slots is not allowed, as it breaks consistency with the other slots.
#' \code{get_*} just returns the value in question. 
#' 
#' @details There are methods for S3 classes defined in the package. See 
#' documentation for the generator functions for these classes (which in 
#' all cases have the same name as the class) for lists of slots for each 
#' class.
#'
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#'  
#' @seealso \code{\link{vreq}}
#' 
#' @example
#' set_com("test")
#'
#' @name setget_methods
NULL
#> NULL

#' @rdname vreq_methods
#' @export
summary.vreq<-function(object,...)
{
  res<-list(class="vreq",
            com=get_com(object),
            comnull=get_comnull(object),
            vr=get_vr(object))

  #a summary_tsvr object inherits from the list class, but has its own print method
  class(res)<-c("summary_tsvr","list")
  return(res)
}

#' @rdname vreq_methods
#' @export
print.vreq<-function(x,...)
{
  cat(paste0("Object of class vreq:\n com: ",get_com(x),"\n comnull: ",get_comnull(x),"\n vr: ",get_vr(x)))
}

#' @rdname setget_methods
#' @export
set_com<-function(obj,newval)
{
  UseMethod("set_com",obj)
}

#' @rdname setget_methods
#' @export
set_com.default<-function(obj,newval)
{
  stop("Error in set_com: set_com not defined for this class")
}

#' @rdname vreq_methods
#' @export
set_com.vreq<-function(obj,newval)
{
  stop("Error in set_com: vreq slots should not be changed individually")
}

#' @rdname setget_methods
#' @export
set_comnull<-function(obj,newval)
{
  UseMethod("set_comnull",obj)
}

#' @rdname setget_methods
#' @export
set_comnull.default<-function(obj,newval)
{
  stop("Error in set_comnull: set_comnull not defined for this class")
}

#' @rdname vreq_methods
#' @export
set_comnull.vreq<-function(obj,newval)
{
  stop("Error in set_comnull: vreq slots should not be changed individually")
}

#' @rdname setget_methods
#' @export
set_vr<-function(obj,newval)
{
  UseMethod("set_vr",obj)
}

#' @rdname setget_methods
#' @export
set_vr.default<-function(obj,newval)
{
  stop("Error in set_vr: set_vr not defined for this class")
}

#' @rdname vreq_methods
#' @export
set_vr.vreq<-function(obj,newval)
{
  stop("Error in set_vr: vreq slots should not be changed individually")
}

#' @rdname setget_methods
#' @export
get_com<-function(obj)
{
  UseMethod("get_com",obj)
}

#' @rdname setget_methods
#' @export
get_com.default<-function(obj)
{
  stop("Error in get_com: get_com not defined for this class")
}

#' @rdname vreq_methods
#' @export
get_com.vreq<-function(obj)
{
  return(obj$com)
}

#' @rdname setget_methods
#' @export
get_comnull<-function(obj)
{
  UseMethod("get_comnull",obj)
}

#' @rdname setget_methods
#' @export
get_comnull.default<-function(obj)
{
  stop("Error in get_comnull: get_comnull not defined for this class")
}

#' @rdname vreq_methods
#' @export
get_comnull.vreq<-function(obj)
{
  return(obj$comnull)
}

#' @rdname setget_methods
#' @export
get_vr<-function(obj)
{
  UseMethod("get_vr",obj)
}

#' @rdname setget_methods
#' @export
get_vr.default<-function(obj)
{
  stop("Error in get_vr: get_vr not defined for this class")
}

#' @rdname vreq_methods
#' @export
get_vr.vreq<-function(obj)
{
  return(obj$vr)
}

