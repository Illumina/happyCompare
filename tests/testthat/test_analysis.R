context("analysis.R")

samplesheet_path <- system.file("extdata/samplesheets", "pcrfree_vs_nano.csv", package = "happyCompare")
happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE)

test_that("extract.happy_compare works", {
  s <- extract(happy_compare, table = "summary")
  expect_true("happy_summary" %in% class(s))
  expect_true(all(c(".Id", "Group.Id", "Sample.Id", "Replicate.Id") %in% names(s)))
  
  e <- extract(happy_compare, table = "extended")
  expect_true("happy_extended" %in% class(e))
  expect_true(all(c(".Id", "Group.Id", "Sample.Id", "Replicate.Id") %in% names(e)))
})

test_that("estimate_hdi works", {
  # NOTE: functional testing only, accuracy tests performed separately with simulated data
  
  d <- happy_extended
  successes_col <- "TRUTH.TP"
  totals_col <- "TRUTH.TOTAL"
  group_cols <- c("Group.Id", "Subset", "Type", "Subtype")
  e1 <- estimate_hdi(happy_extended = d, successes_col = successes_col, totals_col = totals_col, 
    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000)
  e2 <- estimate_hdi(happy_extended = d, successes_col = successes_col, totals_col = totals_col, 
    group_cols = group_cols, aggregate_only = FALSE, sample_size = 1000)
  
  # happy_extended contains subsets with truth.total == 0, check that they've been
  # excluded from output
  expect_equal(dim(e1 %>% filter(replicate_id != ".aggregate"))[1], dim(d)[1] - 
    dim(d %>% filter(TRUTH.TOTAL == 0))[1])
  expect_equal(dim(e2 %>% filter(replicate_id != ".aggregate"))[1], dim(d)[1] - 
    dim(d %>% filter(TRUTH.TOTAL == 0))[1])
  
  # check output cols
  required_cols <- c(group_cols, "replicate_id", "successes", "totals", "mu", "sigma", 
    "alpha0", "beta0", "alpha1", "beta1", "lower", "observed_p", "estimated_p", 
    "upper", "hdi_range")
  expect_true(all(required_cols %in% colnames(e1)))
  expect_true(all(required_cols %in% colnames(e2)))
  
  # check that an aggregate replicate has been added
  expect_true(".aggregate" %in% e1$replicate_id)
  expect_true(".aggregate" %in% e2$replicate_id)
  
  # check that aggregate arg works as intended
  expect_equal(dim(e1 %>% filter(!is.na(lower)))[1], 96)
  expect_equal(dim(e2 %>% filter(!is.na(lower)))[1], 768)
})

test_that(".estimate_hdi works", {
  s <- c(2, 2, 2)
  t <- c(2, 2, 2)
  e1 <- .estimate_hdi(successes = s, totals = t)
  e2 <- .estimate_hdi(successes = s, totals = t, aggregate_only = FALSE)
  
  expect_equal(dim(e1)[1], 4)
  expect_equal(dim(e2)[1], 4)
  
  # additional tests handled by estimate_hdi above
})
