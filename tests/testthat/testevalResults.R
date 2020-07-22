load("../../data/resultstest.rda")
load("../../data/keypostedtest.rda")

evaluations <- evalResults(results = resultstest,
                           key = keypostedtest,
                           type = "R4WSI")

test_that("Evaluations in the correct format",{
  expect_length(evaluations, 3)
  expect_is(evaluations$`Gold-standard HIT Correct Rate by Workers`,
            "table")
})
