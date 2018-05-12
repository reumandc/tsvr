context("vreq_classic_methods")
library(tsvr)

test_that("test the summary method",{
  set.seed(101)
  X<-matrix(runif(10*20)+1,10,20)
  inp<-vreq_classic(X)
  out<-summary(inp)
  expect_equal(names(out),c("CVcom2","CVcomip2","Classic_vr"))
})

test_that("test the print method",{
  set.seed(101)
  X<-matrix(runif(10*20)+1,10,20)
  inp<-vreq_classic(X)
  expect_output(print(inp),"Object of class vreq_classic")
  expect_output(print(inp),"CVcom2: ")
  expect_output(print(inp),"CVcomip2: ")
  expect_output(print(inp),"classic vr: ")
})

