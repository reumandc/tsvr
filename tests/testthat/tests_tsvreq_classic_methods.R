context("tsvreq_classic_methods")
library(tsvr)

test_that("test the summary method",{
  set.seed(101)
  X<-matrix(runif(10*20)+1,10,20)
  inp<-tsvreq_classic(X)
  out<-summary(inp)
  expect_equal(names(out),c("ts","CVcom2","CVcomip2","Classic_vr","wts"))
})

test_that("test the print method",{
  set.seed(101)
  X<-matrix(runif(10*20)+1,10,20)
  inp<-tsvreq_classic(X)
  expect_output(print(inp),"Object of class tsvreq_classic:")
  expect_output(print(inp),"ts: ")
  expect_output(print(inp),"CVcom2: ")
  expect_output(print(inp),"CVcomip2: ")
  expect_output(print(inp),"classic vr: ")
  expect_output(print(inp),"wts: ")
})
