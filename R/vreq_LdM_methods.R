#Simple methods for the vreq_LdM class

#value setting - not necessary, inherited from vreq class
#value getting - not necessary, inherited from vreq class

summary.vreq_LdM<-function(obj)
{
  return(c(CVcom2=get_com(obj),CVpop2=get_comnull(obj),LdM_vr=get_vr(obj)))
}

print.vreq_LdM<-function(obj)
{
  cat(paste0("Object of class vreq_LdM:\n CVcom2: ",get_com(obj),"\n CVpop2: ",get_comnull(obj),"\n LdM vr: ",get_vr(obj)))
}
