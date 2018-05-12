context("vreq_LdM_methods")
library(tsvr)

test_that("test the summary method",{
  set.seed(101)
  X<-matrix(runif(10*20)+1,10,20)
  inp<-vreq_LdM(X)
  out<-summary(inp)
  expect_equal(names(out),c("CVcom2","CVpop2","LdM_vr"))
})

test_that("test the print method",{
  set.seed(101)
  X<-matrix(runif(10*20)+1,10,20)
  inp<-vreq_LdM(X)
  expect_output(print(inp),"Object of class vreq_LdM")
  expect_output(print(inp),"CVcom2: ")
  expect_output(print(inp),"CVpop2: ")
  expect_output(print(inp),"LdM vr: ")
})

