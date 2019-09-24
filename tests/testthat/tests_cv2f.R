context("cv2f")

test_that("test error catching",{
  X<-matrix(runif(10*20)+1,20,20)
  expect_error(cv2f(X,"test"),"Error in cv2f: type must be com, comip")
})

test_that("test that it works for CVcom2",{
  type<-"com"
  X<-matrix(sample(1:10, 10, replace = T),2,5)
  z<-cv2f(X, type)
  z.classic<-cv2(X, type)
  expect_equal(sum(z$cv2), z.classic)
})

test_that("test that it works for CVcomip",{
  type<-"comip"
  X<-matrix(sample(1:10, 10, replace = T),2,5)
  z<-cv2f(X, type)
  z.classic<-cv2(X, type)
  expect_equal(sum(z$cv2), z.classic)
})

test_that("test a specific case that created a bug in a previous version",{
  #The previous version had a bug because it returned a value of CV_com^2, 
  #for certain inputs, that had a negative value (very slightly) at one
  #frequency. This was because the spectrum of the total pop that is in the
  #numerator of CV_com^2 was computed as a sum of cospectra, and numeric
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
    res<-cv2f(Xp,"com")
    expect_true(all(res$cv2>=0))
    expect_equal(length(res$frequency),length(res$cv2))
})
