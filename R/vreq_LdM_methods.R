#Simple methods for the vreq_LdM class

#value setting - not necessary, inherited from vreq class
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

