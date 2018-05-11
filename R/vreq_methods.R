#Simple methods for the vreq class

#value setting - throws and error because we do not want the user to change individual elements
#as that will break the equation
set_com<-function(obj,newval)
{
  UseMethod("set_com",obj)
}

set_com.default<-function(obj,newval)
{
  stop("Error in set_com: set_com only defined for classes vreq and tsvreq")
}

set_com.vreq<-function(obj,newval)
{
  stop("Error in set_com: vreq slots should not be changed individually")
}

set_comnull<-function(obj,newval)
{
  UseMethod("set_comnull",obj)
}

set_comnull.default<-function(obj,newval)
{
  stop("Error in set_comnull: set_comnull only defined for classes vreq and tsvreq")
}

set_comnull.vreq<-function(obj,newval)
{
  stop("Error in set_comnull: vreq slots should not be changed individually")
}

set_vr<-function(obj,newval)
{
  UseMethod("set_vr",obj)
}

set_vr.default<-function(obj,newval)
{
  stop("Error in set_vr: set_vr only defined for classes vreq and tsvreq")
}

set_vr.vreq<-function(obj,newval)
{
  stop("Error in set_vr: vreq slots should not be changed individually")
}

#value getting
get_com<-function(obj)
{
  UseMethod("get_com",obj)
}

get_com.default<-function(obj)
{
  stop("Error in get_com: get_com only defined for classes vreq and tsvreq")
}

get_com.vreq<-function(obj)
{
  return(obj$com)
}

get_comnull<-function(obj)
{
  UseMethod("get_comnull",obj)
}

get_comnull.default<-function(obj)
{
  stop("Error in get_comnull: get_comnull only defined for classes vreq and tsvreq")
}

get_comnull.vreq<-function(obj)
{
  return(obj$comnull)
}

get_vr<-function(obj)
{
  UseMethod("get_vr",obj)
}

get_vr.default<-function(obj)
{
  stop("Error in get_vr: get_vr only defined for class vreq")
}

get_vr.vreq<-function(obj)
{
  return(obj$vr)
}

summary.vreq<-function(obj)
{
  return(c(com=get_com(obj),comnull=get_comnull(obj),vr=get_vr(obj)))
}

print.vreq<-function(obj)
{
  print("Object of class vreq:")
  print(paste0("com: ",com))
  print(paste0("comnull: ",comnull))
  print(paste0("vr: ",vr))
  summary.vreq(obj)  
}

