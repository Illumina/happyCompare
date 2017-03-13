context("happy_summary")

test_that("summary.happy_summary works", {
  hrl = happy_result_list
  
  hs = happyR::extract(happy_result_list = hrl, item = "summary")
  expect_equal(class(hs)[1], "happy_summary")
  expect_equal(dim(hs)[1], 8)
  expect_equal(dim(hs)[2], 19)
  
  s = summary(hs, type = 'SNP', aggregate = FALSE)
  expect_is(s, "knitr_kable")
  expect_equal(length(s), 4)
  
  colnames = c("Sample", "Recall", "Precision", "Frac.NA", "F1.Score", "#QUERY", "TP", "FN", "FP", "UNK")
  s = summary(hs, type = 'SNP', aggregate = FALSE, colnames = colnames)
  expect_equal(length(s), 4)
  
  # alter data to reduce footprint
  tmp = hs %>% mutate(Group.Id = "A")
  class(tmp) = c("happy_summary", class(tmp))
  summary(tmp, type = "SNP", aggregate = TRUE)
})
