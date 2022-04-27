load("../testdata/masstest.rda")
load("../testdata/heldouttest.rda")
load("../testdata/stmPreptest.rda")
load("../testdata/modtest.rda")

test_that("Test class",{
  WItask <- validateTopic(type = "WI", n = 5, vocab = masstest[[1]], beta = masstest[[2]])

  expect_is(WItask, "matrix")
})

test_that("Test topic index",{
  R4WSI0task <- validateTopic(type = "R4WSI0", n = 15,
                             vocab = masstest[[1]],
                             beta = masstest[[2]],
                             text = stmPreptest$meta$post_text[-heldouttest$missing$index],
                             theta = modtest$theta[-heldouttest$missing$index,])

  expect_is(R4WSI0task, "matrix")
  expect_equal(colnames(R4WSI0task)[1], "topic")
  expect_equal(colnames(R4WSI0task)[2], "doc")
  expect_equal(colnames(R4WSI0task)[3], "opt1")
  expect_equal(colnames(R4WSI0task)[4], "opt2")
  expect_equal(colnames(R4WSI0task)[5], "opt3")
  expect_equal(colnames(R4WSI0task)[6], "optcrt")
  expect_equal(length(unique(R4WSI0task[,1])), nrow(masstest[[1]]))
})

