#' Basic methods for the \code{tsvreq} class
#' 
#' Set, get, summary, print and plot methods for the \code{tsvreq} class.
#' 
#' @param object,x,obj An object of class \code{tsvreq}
#' @param newval A new value, for the \code{set_*} methods
#' @param filename A filename, no extension, could have a path. Used for saving a plot as a pdf. The default value NA causes the default plotting device to be used. 
#' @param ... Passed to plot. Not currently used for other methods, included there only for argument consistency
#' with existing generics.
#' 
#' @return \code{summary.tsvreq} produces a summary of a \code{tsvreq} object.
#' Methods \code{print.tsvreq} and \code{plot.tsvreq} are also available. For \code{tsvreq} objects, 
#' \code{set_*} and \code{get_*} methods are available for all slots (see
#' the documentation for \code{tsvreq} for a list). The \code{set_*} methods 
#' just throw an error, to prevent breaking the consistency between the 
#' slots of a \code{tsvreq} object.
#'  
#' @author Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @seealso \code{\link{tsvreq}}
#' 
#' @examples
#' res<-tsvreq(ts=1:10,com=2*c(1:10),comnull=1:10,tsvr=rep(2,10),wts=rep(3,10))
#' get_ts(res)
#' print(res)
#' summary(res)
#' plot(res)
#'  
#' @name tsvreq_methods
NULL
#> NULL

#' @rdname tsvreq_methods
#' @export
summary.tsvreq<-function(object,...)
{
  res<-list(class="tsvreq",
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

#' @rdname tsvreq_methods
#' @export
print.tsvreq<-function(x,...)
{
  cat("Object of class tsvreq:\n")
  cat(" ts, a length",length(x$ts),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$ts,3)),"\n")  
  }else
  {
    cat(paste(signif(x$ts[1:3],3)),"...",paste(signif(x$ts[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
  cat(" com, a length",length(x$com),"numeric vector: ")
  if (length(x$ts)<=7)
  {
    cat(paste(signif(x$com,3)),"\n")  
  }else
  {
    cat(paste(signif(x$com[1:3],3)),"...",paste(signif(x$com[(length(x$ts)-2):(length(x$ts))],3)),"\n")
  }
  cat(" comnull, a length",length(x$comnull),"numeric vector: ")
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

#' @rdname tsvreq_methods
#' @export
#' @importFrom graphics plot par rect lines axis mtext
#' @importFrom grDevices dev.off pdf
plot.tsvreq<-function(x,filename=NA,...)
{
  #plot dimnsions, units inches
  panwd<-3
  panht<-2
  xht<-.25
  numhtwd<-0.25
  ywd<-.25
  gap<-0.25
  totht<-4*(panht+gap)+xht+numhtwd
  totwd<-ywd+numhtwd+panwd+gap
  
  if (!(is.na(filename)))
  {
    grDevices::pdf(file=paste0(filename,".pdf"),width=totwd,height=totht)
  }
  
  #top panel - comnull
  graphics::par(fig=c((ywd+numhtwd)/totwd,
            (ywd+numhtwd+panwd)/totwd,
            (xht+numhtwd+3*(panht+gap))/totht,
            (xht+numhtwd+3*(panht+gap)+panht)/totht),
      mai=c(0,0,0,0),mgp=c(3,.15,0),tcl=-.25)
  xv<-x$ts
  yv<-x$comnull
  graphics::plot(xv,yv,type='l',xaxt="n",ylab="comnull",...)
  graphics::mtext("comnull",side=2,1)
  graphics::axis(1,labels=FALSE)
  
  #next panel down - tsvr
  graphics::par(fig=c((ywd+numhtwd)/totwd,
            (ywd+numhtwd+panwd)/totwd,
            (xht+numhtwd+2*(panht+gap))/totht,
            (xht+numhtwd+2*(panht+gap)+panht)/totht),
      mai=c(0,0,0,0),mgp=c(3,.15,0),tcl=-.25,new=T)
  yv<-x$tsvr
  graphics::plot(xv,yv,type='l',xaxt="n",...)
  graphics::mtext("tsvr",side=2,1)
  graphics::axis(1,labels=FALSE)
  
  #next panel down - com
  graphics::par(fig=c((ywd+numhtwd)/totwd,
            (ywd+numhtwd+panwd)/totwd,
            (xht+numhtwd+1*(panht+gap))/totht,
            (xht+numhtwd+1*(panht+gap)+panht)/totht),
      mai=c(0,0,0,0),mgp=c(3,.15,0),tcl=-.25,new=T)
  yv<-x$com
  graphics::plot(xv,yv,type='l',xaxt="n",...)
  graphics::mtext("com",side=2,1)
  graphics::axis(1,labels=FALSE)
  
  #bottom panel - wts
  graphics::par(fig=c((ywd+numhtwd)/totwd,
            (ywd+numhtwd+panwd)/totwd,
            (xht+numhtwd+0*(panht+gap))/totht,
            (xht+numhtwd+0*(panht+gap)+panht)/totht),
      mai=c(0,0,0,0),mgp=c(3,.15,0),tcl=-.25,new=T)
  yv<-x$wts
  graphics::plot(xv,yv,type='l',xaxt="n",...)
  graphics::mtext("wts",side=2,1)
  graphics::mtext("Timescale",side=1,1)
  graphics::axis(1)
  
  if (!(is.na(filename)))
  {
    grDevices::dev.off()
  }
}

#' @rdname setget_methods
#' @export
set_ts<-function(obj,newval)
{
  UseMethod("set_ts",obj)  
}

#' @rdname setget_methods
#' @export
set_ts.default<-function(obj,newval)
{
  stop("Error in set_ts: set_ts not defined for this class")
}

#' @rdname tsvreq_methods
#' @export
set_ts.tsvreq<-function(obj,newval)
{
  stop("Error in set_ts: tsvreq slots should not be changed individually")
}

#' @rdname tsvreq_methods
#' @export
set_com.tsvreq<-function(obj,newval)
{
  stop("Error in set_com: tsvreq slots should not be changed individually")
}

#' @rdname tsvreq_methods
#' @export
set_comnull.tsvreq<-function(obj,newval)
{
  stop("Error in set_comnull: tsvreq slots should not be changed individually")
}

#' @rdname setget_methods
#' @export
set_tsvr<-function(obj,newval)
{
  UseMethod("set_tsvr",obj)
}

#' @rdname setget_methods
#' @export
set_tsvr.default<-function(obj,newval)
{
  stop("Error in set_tsvr: set_tsvr not defined for this class")
}

#' @rdname tsvreq_methods
#' @export
set_tsvr.tsvreq<-function(obj,newval)
{
  stop("Error in set_tsvr: tsvreq slots should not be changed individually")
}

#' @rdname setget_methods
#' @export
set_wts<-function(obj,newval)
{
  UseMethod("set_wts",obj)
}

#' @rdname setget_methods
#' @export
set_wts.default<-function(obj,newval)
{
  stop("Error in set_wts: set_wts not defined for this class")
}

#' @rdname tsvreq_methods
#' @export
set_wts.tsvreq<-function(obj,newval)
{
  stop("Error in set_wts: tsvreq slots should not be changed individually")
}

#' @rdname setget_methods
#' @export
get_ts<-function(obj)
{
  UseMethod("get_ts",obj)
}

#' @rdname setget_methods
#' @export
get_ts.default<-function(obj)
{
  stop("Error in get_ts: get_ts not defined for this class")
}

#' @rdname tsvreq_methods
#' @export
get_ts.tsvreq<-function(obj)
{
  return(obj$ts)
}

#' @rdname tsvreq_methods
#' @export
get_com.tsvreq<-function(obj)
{
  return(obj$com)
}

#' @rdname tsvreq_methods
#' @export
get_comnull.tsvreq<-function(obj)
{
  return(obj$comnull)
}

#' @rdname setget_methods
#' @export
get_tsvr<-function(obj)
{
  UseMethod("get_tsvr",obj)
}

#' @rdname setget_methods
#' @export
get_tsvr.default<-function(obj)
{
  stop("Error in get_tsvr: get_tsvr not defined for this class")
}

#' @rdname tsvreq_methods
#' @export
get_tsvr.tsvreq<-function(obj)
{
  return(obj$tsvr)
}

#' @rdname setget_methods
#' @export
get_wts<-function(obj)
{
  UseMethod("get_wts",obj)
}

#' @rdname setget_methods
#' @export
get_wts.default<-function(obj)
{
  stop("Error in get_wts: get_wts not defined for this class")
}

#' @rdname tsvreq_methods
#' @export
get_wts.tsvreq<-function(obj)
{
  return(obj$wts)
}
