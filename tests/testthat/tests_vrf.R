context("vrf")

test_that("test for correct format",{
  set.seed(101)
  X<-matrix(runif(10*100),10,100)
  res<-vrf(X)  
  expect_equal(class(res),"list")
  expect_equal(names(res),c("frequency","vr"))
  expect_equal(res$frequency,(1:99)/100)
})

test_that("test for correctvalue",{
  set.seed(102)
  X<-matrix(runif(2*100),2,100)
  res<-vrf(X)  
  fft1<-fft(X[1,])
  fft1<-fft1[2:length(fft1)]
  fft2<-fft(X[2,])
  fft2<-fft2[2:length(fft2)]
  cosp11<-(Mod(fft1))^2
  cosp22<-(Mod(fft2))^2
  cosp12<-fft1*Conj(fft2)
  cosp21<-fft2*Conj(fft1)
  hres<-(cosp11+cosp22+cosp12+cosp21)/(cosp11+cosp22)
  expect_equal(res$vr,Re(hres))
})

test_that("test a specific case that created a bug in a previous version",{
  #The previous version had a bug because it returned a value of of the ts-specific vr, 
  #for certain inputs, that had a negative value (very slightly) at one
  #frequency. This was because the spectrum of the total pop that is in the
  #numerator of was computed as a sum of cospectra, and numeric
  #error could result in these being very slightly negative sometimes.
  
  Xp<-matrix(c(1,1,1.25,1.5,1.75,1.75,0.75,0.25,0.25,0.75,1,0,1.5,2,0.5,1.75,0.75,2,0,1.25,1.5,1.5,0.25,2,4.25,2.25,0,0,
               0,0,0,0,0.75,0,0,0,0.75,0,0.5,0,0,0,0.25,0,0.25,0,0,0,0.25,0,0.75,0.5,0,0.5,0.25,0.5,
               0,0,0.5,0,0,0.25,0,0,0,0,0,1,1.75,0,0,2.5,0,0,1.25,0,0,0,0,0,1.5,1.25,0,0,
               1.5,2,2.75,2.75,1.5,8,2.75,1.5,2,2.25,1.5,2,2,4,3.5,2,2,2,1.5,2.75,2.75,3.5,2,7.5,2,1.5,3.5,2,
               0.25,0,0.5,0,0,2,0.25,0,0,0,0,7.5,30,0,0.5,1,1,0,0,0,0,0,0,0,12.5,6.5,0,0.25,
               15,1.5,18.75,37.5,25,21.25,13.75,11.25,25,10,25,13.75,16.25,13.75,30,15,8.75,10,6.25,40,20,8.75,2.75,13,20,22.5,45,47.5,
               2.25,4.25,2,0.5,2.25,10,1.5,6.75,10.5,6.75,2.75,0.5,0.5,1.75,10.5,1,3.5,0.75,13.75,4.75,1.5,4,2,0,0.75,1,1,1,
               8.5,2.5,2.5,3.75,1.5,5.5,4.25,1,5,12.5,3.75,0.25,1.25,6.25,3.75,0.5,5.5,2.75,0.5,10,3.75,4.25,2.5,2.5,5.5,2.5,1.75,1.25,
               0,0,0,0,0,0.5,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0.5,0,0,0.75,0,
               2.75,1.25,0,0.5,0,0,0,1.25,1,0.25,0,0,0,0.5,0,0,1.25,0,0,0,0,1.75,1.25,0,1.25,0,0,0.5,
               0,0,0,1,0.25,0,0,0,0,0,0.25,0,0,0,0,0,0.25,0,0,0,0,0,0,0,0,0,0,0,
               0.25,0,0,0,0,0,0.25,0,0,0.25,0,0,0,0,0,0,0,0,0,0,0,0.25,0,0,0,0,0,0,
               1.25,0.25,0,0,1,0.25,1.75,0.5,0.25,3,0.5,1.75,0.5,0.25,0,0.5,0.5,1,0,0,0.25,1,0,0.5,0,0,0.5,0,
               2.5,0.5,2.25,4.75,0.25,5,0.5,0.5,1.25,7.5,11.25,5.5,3.5,1.25,0.5,10,2,0,2.25,2.75,0.25,2.5,1.75,1.75,11.75,8,5.5,1.25,
               52.5,17.5,20,10,10,12.5,37.5,17.5,25,36.25,7.5,8.75,11.25,13.75,12.5,18.75,30,6,11.25,22.5,7.25,30,17.5,6,11.25,20,5.5,22.5,
               0.5,0.25,0,0,0,0,0,0.5,1,0.25,0,2,0,0,0,4,0,0,1.5,0,0,0,0,0.25,0,0,0.25,0,
               1,0,0.5,1.75,1,4.25,0.25,0,0.5,0.25,1,3,1.75,1,0.5,1,0.5,0,0.5,1,0.75,0.75,0.75,1,1.75,1,1.25,1,
               3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
               2.25,60,3,0,2.75,0.5,0.25,21.25,9.25,2.25,0.25,2.5,1.75,0,0.5,4,0.5,0.5,4.75,0.5,1.75,0.25,32.5,18.75,7.5,13,0.75,0,
               0,0,0,0,0,0,0.25,1.25,0.25,0,0,0,0,0,0,2.5,0,0,0,0,0.25,0,0,0,0,2.5,0,0,
               13.75,2,12.5,30,42.5,30,27.5,8.75,6.75,16.25,40,22.5,10.5,55,37.5,30,30,50,55,15,45,27.5,17.5,25,5.5,12.5,17.5,3.5,
               0.5,0,0,1.25,0.5,0.5,0.75,0.25,1.25,0,1.5,0,0,0,0,0.5,1,0,0,1.25,0.25,0.5,0.25,1.25,0,0,1,0.5,
               2,0,0.5,0,2.25,1.25,0.75,1,2,0.5,0.5,1.75,0.5,1.5,1,2.25,0.25,0,0,0,0,0.5,0.5,1.5,0.25,2.25,5.5,8.75),23,28,byrow = TRUE)
  rownames(Xp)<-c("agoseris heterophylla","astragalus gambelianus","bombycilaena californica","brodiaea sp",
                  "bromus hordeaceus","calycadenia multiglandulosa","castilleja densiflora",
                  "chlorogalum pomeridianum","crassula connata","elymus multisetus",
                  "epilobium brachycarpum","eschscholzia californica","hemizonia congesta",
                  "hesperevax sparsiflora","lasthenia californica","layia platyglossa",
                  "lotus wrangelianus","melica californica","microseris douglasii","nassella pulchra",
                  "plantago erecta","poa secunda","vulpia microstachys")
  colnames(Xp)<-c("1983","1993","2004","1999","1989","2002","1987","1994","1995","1984","1998",
                  "2008","2007","2000","2001","2009","1985","1988","2010","2003","1990","1986","1992",
                  "1991","2006","2005","1997","1996")
  res<-vrf(Xp)
  expect_true(all(res$vr>=0))
})

