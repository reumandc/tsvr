#Simple methods for the vreq_LdM class

#value setting - these just throw an error, since we do not want
#individual components of a vreq_LdM object changed as that breaks the
#consistency among the components
set_com.vreq_LdM<-function(obj,newval)
{
  stop("Error in set_com: com should not be altered for a vreq_LdM object")
}

set_comnull.vreq_LdM<-function(obj,newval)
{
  stop("Error in set_comnull: comnull should not be altered for a vreq_LdM object")
}

set_vr.vreq_LdM<-function(obj,newval)
{
  stop("Error in set_vr: vr should not be altered for a vreq_LdM object")
}

#value getting - not necessary, inherited from vreq class

summary.vreq_LdM<-function(obj)
{
  print("Object of class vreq_LdM - an equation using the Loreau-de Mazancourt variance ratio:")
  print(paste0("CV_com^2: ",com))
  print(paste0("CV_pop^2: ",comnull))
  print(paste0("variance ratio: ",vr))
}

print.vreq_LdM<-function(obj)
{
  summary.vreq_LdM(obj)  
}

