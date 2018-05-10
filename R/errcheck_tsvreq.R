#' Error check inputs for the creator function for the \code{tsvreq} class.
#' 
#' @param ts Timescales, should be a numeric vector of nonnegative numbers 
#' @param com Should be a vector of nonnegative numbers, equal to \code{comnull}*\code{tsvr}
#' @param comnull Should be a vector of nonnegative numbers
#' @param tsvr Should be a vector of nonnegative numbers
#' @param wts Should be a vector of nonnegative numbers
#' 
#' @return \code{errcheck_tsvreq} returns nothing but throws and error if the inputs do not
#' meet the requirements of a \code{tsvreq} object
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}

errcheck_tsvreq<-function(ts,com,comnull,tsvr,wts)
{
  #numeric, single numbers, no NAs
  if (!(is.numeric(ts) && is.numeric(com) && is.numeric(comnull) && is.numeric(tsvr) && is.numeric(wts)))
  {
    stop("Error in tsvreq class: all slots must be numeric")
  }
  if (!(length(ts)==length(com) && length(ts)==length(comnull) && 
        length(ts)==length(tsvr) && length(wts)==length(ts)))
  {
    stop("Error in tsvreq class: all slots must be the same length")
  }
  if (!(all(is.finite(ts)) && all(is.finite(com)) && all(is.finite(comnull)) && 
        all(is.finite(tsvr)) && all(is.finite(wts))))
  {
    stop("Error in tsvreq class: non-finite values not allowed")
  }
  
  #nonnegativity
  if (any(ts<0) || any(com<0) || any(comnull<0) || any(tsvr<0) || any(wts<0))
  {
    stop("Error in tsvreq class: negative values not allowed")
  }
  
  #equation satisfied
  if (!isTRUE(all.equal(com,comnull*tsvr)))
  {
    stop("Error in tsvreq class: com should equal comnull times tsvr")
  }
}