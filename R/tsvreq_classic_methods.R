#' Basic methods for the \code{tsvreq_classic} class
#' 
#' Set, get, summary, print and plot methods for the \code{tsvreq_classic} class.
#' 
#' @param object,x,obj An object of class \code{tsvreq_classic}
#' @param newval A new value, for the \code{set_*} methods
#' @param ... Not currently used. Included for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.tsvreq_classic} produces a summary of a \code{tsvreq_classic} object.
#' Methods \code{print.tsvreq_classic} and \code{plot.tsvreq_classic} are also available. 
#' For \code{tsvreq_classic} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{tsvreq_classic} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{tsvreq_classic} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{tsvreq_classic}}
#' 
#' @examples
#' add later
#'  
#' @name tsvreq__classic_methods
NULL
#> NULL

#' @rdname tsvreq_classic_methods
#' @export
summary.tsvreq_classic<-function(object,...)
{
  res<-list(class="tsvreq_classic",
            ts_start=object$ts[1],
            ts_end=object$ts[length(object$ts)],
            ts_length=length(object$ts),
            com_length=length(object$com),
            comnull_length=length(object$comnull),
            tsvr_length=length(object$tsvr),
            wts_length=length(object$tsvr))
  
  #a summary_tsvr object inherits from the list class, but has its own print method
  class(res)<-c("summary_tsvr","list")
  return(res)
}

#' @rdname tsvreq_classic_methods
#' @export
print.tsvreq_classic<-function(x,...)
{
  cat("Object of class tsvreq_classic:\n")
  cat(" ts, a length",length(x$ts),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$ts,3)),"\n")  
  }else
  {
    cat(paste(signif(x$ts[1:3],3)),"...",paste(signif(x$ts[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
  cat(" CVcom2, a length",length(x$com),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$com,3)),"\n")  
  }else
  {
    cat(paste(signif(x$com[1:3],3)),"...",paste(signif(x$com[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
  cat(" CVcomip2, a length",length(x$comnull),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$comnull,3)),"\n")  
  }else
  {
    cat(paste(signif(x$comnull[1:3],3)),"...",paste(signif(x$comnull[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
  cat(" tsvr, a length",length(x$tsvr),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$tsvr,3)),"\n")  
  }else
  {
    cat(paste(signif(x$tsvr[1:3],3)),"...",paste(signif(x$tsvr[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
  cat(" wts, a length",length(x$wts),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$wts,3)),"\n")  
  }else
  {
    cat(paste(signif(x$wts[1:3],3)),"...",paste(signif(x$wts[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
}

#' @rdname tsvreq_classic_methods
#' @export
#we need a plot method, code snippets cannibalized from elsewhere pasted below
#plot.tsvreq_classic<-function(obj)
#{
#

#' @param fig logical. If \code{FALSE} (default), do not output the figure. If \code{TRUE}, 
#' plot the figure with x-axis frequency and y-axis variance ratio.

#if(fig){
#  if(f.flag=="fs"){
#    plot(cospec$frequency, vr.f, xlab="frequency (cycles/year)", ylab="frequency specific VR",
#         xlim=c(0,1), ylim=c(0, max(vr.f)), typ="l")
#  }else{plot(cospec$frequency, vr.f, xlab="frequency (cycles/year)", ylab="frequency decomposition of VR",
#             xlim=c(0,1), ylim=c(0, max(vr.f)), typ="l")
#    text(0.5,max(vr.f),labels=paste("VR=",as.character(round(sum(vr.f),4)),sep=''),adj=c(1.05,-.4))}
#  lines(c(.5,.5),c(0,max(vr.f)),lty='dotted')
#  rect(.5,-max(vr.f),2,2*max(vr.f),density=NA,col=rgb(0,0,0,alpha=0.2))
#}

#' @param fig logical. If \code{FALSE} (default), do not output the figure. If \code{TRUE}, 
#' plot the figure with x-axis frequency and y-axis CV^2.

#if(fig){
#  if(type=="com"){
#    plot(freq, cv2.f, xlab="frequency (cycles/year)", ylab="",
#         xlim=c(0,1), ylim=c(0, max(cv2.f)), typ="l")
#    mtext(expression(paste(CV[com]^2,(italic(f)))), side=2, line=2)
#    text(0.5,max(cv2.f),labels=bquote(paste(CV[com]^2,"=",.(round(sum(cv2.f),4)))),adj=c(1.05,0.4))
#  }else{plot(freq, cv2.f, xlab="frequency (cycles/year)", ylab="",
#             xlim=c(0,1), ylim=c(0, max(cv2.f)), typ="l")
#    mtext(expression(paste(CV[pop]^2,(italic(f)))), side=2, line=2)
#    text(0.5,max(cv2.f),labels=bquote(paste(CV[pop]^2,"=",.(round(sum(cv2.f),4)))),adj=c(1.05,0.4))}
#  
#  lines(c(.5,.5),c(0,max(cv2.f)),lty='dotted')
#  rect(.5,-max(cv2.f),2,2*max(cv2.f),density=NA,col=rgb(0,0,0,alpha=0.2))
#}

#' @param fig logical. If \code{FALSE} (default), do not output the figure. If \code{TRUE}, 
#' plot the figure with x-axis frequency and y-axis power of oscillation

#if(fig){
#  plot(cospec$frequency, strength, xlab="frequency (cycles/year)", ylab="strength of oscillation",
#       xlim=c(0,1), ylim=c(0, max(strength)), typ="l")
#  text(0.5,max(strength),labels=paste("pow=",as.character(round(sum(strength),4)),sep=''),adj=c(1.05,-.4))
#  lines(c(.5,.5),c(0,max(strength)),lty='dotted')
#  rect(.5,-max(strength),2,2*max(strength),density=NA,col=rgb(0,0,0,alpha=0.2))
#}

#}

#' @rdname tsvreq_classic_methods
#' @export
set_ts.tsvreq_classic<-function(obj,newval)
{
  stop("Error in set_ts: tsvreq_classic slots should not be changed individually")
}

#' @rdname tsvreq_classic_methods
#' @export
set_com.tsvreq_classic<-function(obj,newval)
{
  stop("Error in set_com: tsvreq_classic slots should not be changed individually")
}

#' @rdname tsvreq_classic_methods
#' @export
set_comnull.tsvreq_classic<-function(obj,newval)
{
  stop("Error in set_comnull: tsvreq_classic slots should not be changed individually")
}

#' @rdname tsvreq_classic_methods
#' @export
set_tsvr.tsvreq_classic<-function(obj,newval)
{
  stop("Error in set_tsvr: tsvreq_classic slots should not be changed individually")
}

#' @rdname tsvreq_classic_methods
#' @export
set_wts.tsvreq_classic<-function(obj,newval)
{
  stop("Error in set_wts: tsvreq_classic slots should not be changed individually")
}

#' @rdname tsvreq_classic_methods
#' @export
get_ts.tsvreq_classic<-function(obj)
{
  return(obj$ts)
}

#' @rdname tsvreq_classic_methods
#' @export
get_com.tsvreq_classic<-function(obj)
{
  return(obj$com)
}

#' @rdname tsvreq_classic_methods
#' @export
get_comnull.tsvreq_classic<-function(obj)
{
  return(obj$comnull)
}

#' @rdname tsvreq_classic_methods
#' @export
get_tsvr.tsvreq_classic<-function(obj)
{
  return(obj$tsvr)
}

#' @rdname tsvreq_classic_methods
#' @export
get_wts.tsvreq_classic<-function(obj)
{
  return(obj$wts)
}

