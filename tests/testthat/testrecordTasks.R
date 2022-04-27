load("../testdata/allR4WSItasktest.rda")

record <- recordTasks(type = "R4WSI0",
                      tasks = allR4WSItasktest,
                      path = "../testdata/allR4WSItasktest.rda")

test_that("Record exists in path",{
  expect_true(file.exists("../testdata/allR4WSItasktest.rda"))
  # file created within 1 min
  expect_true(Sys.time() - file.info("../testdata/allR4WSItasktest.rda")$ctime < 5)
})

test_that("Record is in the correct format",{
  expect_length(record, 2)
  expect_is(record[[1]], "data.frame")
  expect_is(record[[1]][,6], "character")
  expect_is(record[[1]][,7], "integer")
  expect_equal(ncol(record[[1]]), 7)
  expect_is(record[[2]], "data.frame")
  expect_equal(nrow(record[[1]]), nrow(record[[2]]))
  expect_equal(ncol(record[[2]]), 5)
  expect_is(record[[2]][,1], "character")
  expect_is(record[[2]][,2], "character")
  expect_is(record[[2]][,3], "character")
  expect_is(record[[2]][,4], "character")
  expect_is(record[[2]][,5], "character")
  # check documents are in order
  expect_identical(record[[1]][1, 2], record[[2]][1, 1])
  expect_identical(record[[1]][2, 2], record[[2]][2, 1])
  expect_identical(record[[1]][nrow(record[[1]]), 2], record[[2]][nrow(record[[2]]), 1])
  # word sets are retained
  expect_equal(sum(unique(record[[2]][1, 2:5]) %in% unique(record[[1]][1, 3:6])),
               4)
  expect_equal(sum(unique(record[[2]][nrow(record[[2]]), 2:5]) %in% unique(record[[1]][nrow(record[[1]]), 3:6])),
               4)
  # order of word sets changed
  expect_false(identical(record[[1]][,2:6], record[[2]]))
})
