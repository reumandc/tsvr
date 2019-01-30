#' Print method for \code{summary_tsvr} class
#' 
#' Print method for \code{summary_tsvr} class
#' 
#' @param x A \code{summary_tsvr} object
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{print.summary_tsvr} is called for its effect of
#' printing to the screen.
#' 
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @examples 
#' #Fill in later
#' 
#' @export

print.summary_tsvr<-function(x,...)
{
  for (counter in 1:length(x))
  {
    cat(names(x)[counter],": ",x[[counter]],"\n",sep="")
  }
}

