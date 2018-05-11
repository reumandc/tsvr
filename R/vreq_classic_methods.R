#Simple methods for the vreq_classic class

#value setting - not necessary, inherited from vreq class
#value getting - not necessary, inherited from vreq class

summary.vreq_classic<-function(obj)
{
  print("Object of class vreq_classic - an equation using the classic variance ratio:")
  print(paste0("CV_com^2: ",com))
  print(paste0("CV_comip^2: ",comnull))
  print(paste0("variance ratio: ",vr))
}

print.vreq_classic<-function(obj)
{
  summary.vreq_classic(obj)  
}

