context("happy_summary")

test_that("summary.happy_summary works", {
  hrl = happy_result_list
  
  hs = happyR::extract(happy_result_list = hrl, item = "summary")
  expect_equal(class(hs)[1], "happy_summary")
  expect_equal(dim(hs)[1], 12)
  expect_equal(dim(hs)[2], 18)
  
  s = summary(hs, type = 'SNP', aggregate = FALSE)
  expect_is(s, "knitr_kable")
  expect_equal(length(s), 5)
  
  colnames = c("Sample", "Recall", "Precision", "Frac.NA", "#QUERY", "TP", "FN", "FP", "UNK")
  s = summary(hs, type = 'SNP', aggregate = FALSE, colnames = colnames)
  expect_equal(length(s), 5)
  
  # TODO: add test for aggregate = TRUE when appropriate data available
})
