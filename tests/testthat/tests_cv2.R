context("cv2")
library(tsvr)

test_that("test error catching",{
  X<-matrix(runif(10*20)+1,20,20)
  expect_error(cv2(X,"test"),"Error in cv2: type must be com, comip, or pop")
})

test_that("test cases where it actually provides results",{
  set.seed(401)
  X<-matrix(runif(10*20)+1,10,20)
  h<-cv2(X, "com")
  Xtot<-colSums(X)
  expect_equal(h,(sd(Xtot)/mean(Xtot))^2)
  #h<-cv2(X, "comip")
  #***DAN: finish off
  #h<-cv2(X, "pop")
  
})

