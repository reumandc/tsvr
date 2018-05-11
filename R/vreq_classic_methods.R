#Simple methods for the vreq_classic class

#value setting - not necessary, inherited from vreq class
#value getting - not necessary, inherited from vreq class

summary.vreq_classic<-function(obj)
{
  return(c(CVcom2=get_com(obj),CVcomip2=get_comnull(obj),Classic_vr=get_vr(obj)))
}

print.vreq_classic<-function(obj)
{
  cat(paste0("Object of class vreq_classic:\n CVcom2: ",get_com(obj),"\n CVcomip2: ",get_comnull(obj),"\n classic vr: ",get_vr(obj)))
}

