#Simple methods for the tsvreq class

#value setting
set_ts<-function(obj,newval)
{
  UseMethod("set_ts",obj)  
}

set_ts.default<-function(obj,newval)
{
  stop("Error in set_ts: set_ts only defined for class tsvreq")
}

set_ts.tsvreq<-function(obj,newval)
{
  errcheck_tsvreq(newval,obj$com,obj$comnull,obj$tsvr,obj$wts)
  obj$ts<-newval
  return(obj)
}

set_com.tsvreq<-function(obj,newval)
{
  errcheck_tsvreq(obj$ts,newval,obj$comnull,obj$tsvr,obj$wts)
  obj$com<-newval
  return(obj)
}

set_comnull.tsvreq<-function(obj,newval)
{
  errcheck_tsvreq(obj$ts,obj$com,newval,obj$tsvr,obj$wts)
  obj$comnull<-newval
  return(obj)
}

set_tsvr<-function(obj,newval)
{
  UseMethod("set_tsvr",obj)
}

set_tsvr.default<-function(obj,newval)
{
  stop("Error in set_tsvr: set_tsvr only defined for class tsvreq")
}

set_tsvr.tsvreq<-function(obj,newval)
{
  errcheck_tsvreq(obj$ts,obj$com,obj$comnull,newval,obj$wts)
  obj$tsvr<-newval
  return(obj)
}

#value getting
get_ts<-function(obj)
{
  UseMethod("get_ts",obj)
}

get_ts.default<-function(obj)
{
  stop("Error in get_ts: get_ts only defined for class tsvreq")
}

get_ts.tsvreq<-function(obj)
{
  return(obj$ts)
}

get_com.tsvreq<-function(obj)
{
  return(obj$com)
}

get_comnull.tsvreq<-function(obj)
{
  return(obj$comnull)
}

get_tsvr<-function(obj)
{
  UseMethod("get_tsvr",obj)
}

get_tsvr.default<-function(obj)
{
  stop("Error in get_tsvr: get_tsvr only defined for class tsvreq")
}

get_tsvr.tsvreq<-function(obj)
{
  return(obj$tsvr)
}

get_wts<-function(obj)
{
  UseMethod("get_wts",obj)
}

get_wts.default<-function(obj)
{
  stop("Error in get_wts: get_wts only defined for class tsvreq")
}

get_wts.tsvreq<-function(obj)
{
  return(obj$wts)
}

summary.tsvreq<-function(obj)
{
  print("Object of class tsvreq:")
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

print.tsvreq<-function(obj)
{
  summary.tsvreq(obj)  
}

#we need a plot method
#plot.tsvreq<-function(obj)
#{
#  
#}