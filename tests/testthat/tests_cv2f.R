context("cv2f")
library(tsvr)

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


