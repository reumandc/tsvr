#Simple methods for the vreq_atsclassic class

#set methods - mostly inherited from vreq, one new one needed
set_ts.vreq_atsclassic<-function(obj,newval)
{
  stop("Error in set_ts: vreq_atsclassic slots should not be changed individually")
}

#get methods - mostly inherited from vreq, one new one needed
get_ts.vreq_atsclassic<-function(obj)
{
  return(obj$ts)
}

#***DAN: need summary and print methods