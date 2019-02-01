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
#' res<-vreq(2,1,2)
#' summary(res)
#' 
#' @seealso \code{\link{vreq_methods}}, \code{\link{vreq_classic_methods}}, 
#' \code{\link{vreq_LdM_methods}}, \code{\link{vreq_classic_ag_methods}}, 
#' \code{\link{tsvreq_methods}}, \code{\link{tsvreq_classic_methods}}, 
#' \code{browseVignettes("tsvr")}
#' 
#' @export

print.summary_tsvr<-function(x,...)
{
  for (counter in 1:length(x))
  {
    cat(names(x)[counter],": ",x[[counter]],"\n",sep="")
  }
}

