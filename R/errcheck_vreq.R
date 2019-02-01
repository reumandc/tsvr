#' Error check inputs for the creator function for the \code{vreq} class.
#' 
#' @param com Should be a positive number, equal to \code{comnull}*\code{vr}
#' @param comnull Should be a positive number
#' @param vr Should be a positive number
#' 
#' @return \code{errcheck_vreq} returns nothing but throws and error if the inputs do not
#' meet the requirements of a \code{vreq} object
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_vreq<-function(com,comnull,vr)
{
  #numeric, single numbers, no NAs
  if (!(is.numeric(com) && is.numeric(comnull) && is.numeric(vr)))
  {
    stop("Error in vreq class: all slots must be numeric")
  }
  if (!(length(com)==1 && length(comnull)==1 && length(vr)==1))
  {
    stop("Error in vreq class: all slots must be single numbers")
  }
  if (!(is.finite(com) && is.finite(comnull) && is.finite(vr)))
  {
    stop("Error in vreq class: non-finite values not allowed")
  }
  
  #positivity
  if (com<=0 || comnull<=0 || vr<=0)
  {
    stop("Error in vreq class: only positive values allowed")
  }
  
  #equation satisfied
  if (!isTRUE(all.equal(com,comnull*vr)))
  {
    stop("Error in vreq class: com should equal comnull times vr")
  }
}