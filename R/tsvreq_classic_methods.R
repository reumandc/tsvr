#Simple methods for the tsvreq_classic class

#set methods - inherited
#get methods - inherited

summary.tsvreq_classic<-function(obj)
{
  print("Object of class tsvreq_classic:")
  print("ts:")
  print(ts)
  print("com:")
  print(com)
  print("comnull:")
  print(comnull)
  print("tsvr:")
  print(tsvr)
  print("wts:")
  print(wts)
}

print.tsvreq_classic<-function(obj)
{
  summary.tsvreq_classic(obj)  
}

#we need a plot method, code snippets cannibalized from elsewhere pasted below
#plot.tsvreq_classic<-function(obj)
#{
#  
#}



#need a plotter method, amoung others. Some code swiped that used to be in the creator 
#function is here:

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
