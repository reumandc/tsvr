#' Error check a data matrix.
#' 
#' @param X a matrix with counts or densities arranged in species by years. No NAs or negative values allowed,
#' constant species not allowed.
#' @param calledby the function calling this one
#' 
#' @return \code{errcheck_data} returns nothing but throws and error if the inputs do not
#' meet the requirements
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' X<-matrix(runif(10*100),10,100)
#' errcheck_data(X)

errcheck_data<-function(X,calledby)
{
  if (any(!is.finite(X)) || any(X<0))
  {
    stop(paste0("Error in ",calledby,": data must consist of non-negative numbers, no NAs"))
  }
  if (any(rowSums(X)==0))
  {
    stop(paste0("Error in ",calledby,": data should not contain species that are always 0"))
  }
  for (counter in 1:dim(X)[1])
  {
    h<-X[counter,]
    if (isTRUE(all.equal(diff(range(h)),0)))
    {
      stop(paste0("Error in ",calledby,": data should not contain species with constant abundance/density"))
    }
  }
}

