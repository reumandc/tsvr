context("aggts")

test_that("test format of output",{
  set.seed(101)
  X<-matrix(runif(10*100),10,100)
  tsvr<-tsvreq_classic(X)
  res<-aggts(tsvr,tsvr$ts[tsvr$ts>4])
  expect_s3_class(res,"vreq_classic_ag")
  expect_s3_class(res,"vreq")
  expect_s3_class(res,"list")
  expect_equal(names(res),c("com","comnull","vr","ts"))
  expect_equal(res$com,res$comnull*res$vr)
  expect_equal(res$ts,tsvr$ts[tsvr$ts>4])
})

test_that("test accuracy of output",{
  #aggregate all time scales, see if you get the classic vr equation
  set.seed(101)
  X<-matrix(runif(10*100),10,100)
  tsvr<-tsvreq_classic(X)
  res<-aggts(tsvr,tsvr$ts)
  vrres<-vreq_classic(X)
  expect_equal(vrres$com,res$com) 
  expect_equal(vrres$comnull,res$comnull)
  expect_equal(vrres$vr,res$vr)
})