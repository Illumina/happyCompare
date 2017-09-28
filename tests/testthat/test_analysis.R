context("analysis.R")

samplesheet_path <- system.file("tests/data/pcrfree_vs_nano/pcrfree_vs_nano.tests.csv", 
                                package = "happyCompare")
happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE)

test_that("extract_metrics works", {
  
  e <- extract_metrics(happy_compare, table = "summary")
  expect_is(e, "happy_summary")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", "build_metrics") %in% names(e)))
  expect_equal(dim(e)[1], 16)
  expect_equal(dim(e)[2], 22)
  
  e <- extract_metrics(happy_compare, table = "extended")
  expect_is(e, "happy_extended")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", "build_metrics") %in% names(e)))
  expect_equal(dim(e)[1], 704)
  expect_equal(dim(e)[2], 70)
  
  e <- extract_metrics(happy_compare, table = "pr.indel.pass")
  expect_is(e, "happy_roc")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", "build_metrics") %in% names(e)))
  expect_equal(dim(e)[1], 9126)
  expect_equal(dim(e)[2], 71)
  
  expect_error(suppressWarnings(e <- extract_metrics(happy_compare, table = "pr.snp.pass")))
  
  e <- extract_metrics(happy_compare, table = "build.metrics")
  expect_is(e, "build_metrics")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", "build_metrics") %in% names(e)))
  expect_equal(dim(e)[1], 4)
  expect_equal(dim(e)[2], 140)
  
})

test_that("estimate_hdi works", {
  
  # prepare data
  d <- extract_metrics(happy_compare, table = "extended") %>% 
    dplyr::filter(Type == "INDEL", Subtype == "D1_5", Filter == "PASS")
  expect_is(d, "data.frame")
  expect_equal(dim(d)[1], 32)
  expect_equal(dim(d)[2], 70)

  successes_col <- "TRUTH.TP"
  totals_col <- "TRUTH.TOTAL"
  group_cols <- c("Group.Id", "Subset", "Type", "Subtype")

  # estimate hdis with / without per-replicate results
  expect_warning(e1 <- estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
                                    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000))
  expect_warning(e2 <- estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
                                    group_cols = group_cols, aggregate_only = FALSE, sample_size = 1000))
  
  # check output cols
  required_cols <- c(group_cols, "replicate_id", "successes", "totals", "mu", "sigma", 
                     "alpha0", "beta0", "alpha1", "beta1", "lower", "observed_p", "estimated_p", 
                     "upper", "hdi_range")
  expect_true(all(required_cols %in% colnames(e1)))
  expect_true(all(required_cols %in% colnames(e2)))
  
  # check that an aggregate replicate has been added
  expect_true(".aggregate" %in% e1$replicate_id)
  expect_true(".aggregate" %in% e2$replicate_id)
  
  # check that aggregate_only works as intended
  expect_equal(sum(is.na(e1["lower"])), 24)
  expect_equal(sum(is.na(e2["lower"])), 0)
  
  # check that subsets where TRUTH.TOTAL == 0 have been excluded 
  d_n_subsets <- dim(d)[1]
  d_n_subsets_empty <- sum(d[totals_col] == 0)
  e1_n_subsets <- dim(e1 %>% dplyr::filter(replicate_id != ".aggregate"))[1]
  e2_n_subsets <- dim(e2 %>% dplyr::filter(replicate_id != ".aggregate"))[1]
  expect_equal(d_n_subsets - d_n_subsets_empty, e1_n_subsets)
  expect_equal(d_n_subsets - d_n_subsets_empty, e2_n_subsets)
  
  # check that a warning is returned when user-defined subsets are missing in one of the groups
  # note that some Subsets are defaults in hap.py and will always be present
  # a) only 1 subset missing
  unbalanced_d <- bind_rows(
    d %>% filter(Group.Id == "PCR-Free"),
    d %>% filter(Group.Id == "Nano", Subset != "high.at")
  )
  expect_warning(e1 <- estimate_hdi(df = unbalanced_d, successes_col = successes_col, totals_col = totals_col, 
                                    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000),
                 regexp = "Subsets in input.data frame are not consistent across replicates")
  # b) all user-defined subsets missing
  happy_defaults <- c("*", "TS_boundary", "TS_contained")
  unbalanced_d <- bind_rows(
    d %>% filter(Group.Id == "PCR-Free"),
    d %>% filter(Group.Id == "Nano", Subset %in% happy_defaults)
  )
  expect_warning(e1 <- estimate_hdi(df = unbalanced_d, successes_col = successes_col, totals_col = totals_col, 
                                    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000),
                 regexp = "Subsets in input.data frame are not consistent across replicates")  
  
  # see package vignettes for details on method calibration
  
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

test_that("compare_hdi works", {
  
  # prepare data
  d <- extract_metrics(happy_compare, table = "extended") %>% 
    dplyr::filter(Type == "INDEL", Subtype == "D1_5", Filter == "PASS")
  successes_col <- "TRUTH.TP"
  totals_col <- "TRUTH.TOTAL"
  group_cols <- c("Group.Id", "Subset", "Type", "Subtype")
  e <- suppressWarnings(estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
                    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000))
  hdi_diff <- compare_hdi(happy_hdi = e)
  
  # check data integrity
  expect_is(hdi_diff, "data.frame")
  
  expect_equal(dim(hdi_diff)[1], 6)
  expect_equal(dim(hdi_diff)[2], 6)
  
  required_cols <- c("Subset", "Type", "estimated_diff", "lower", "upper", "significant")
  expect_true(all(colnames(hdi_diff) %in% required_cols))
  
  expect_equal(sum(hdi_diff$significant), 4)
  
})

test_that(".compare_hdi works", {
  
  # no difference
  diff <- .compare_hdi(alpha_a = 100, beta_a = 100, alpha_b = 10, beta_b = 10)
  expect_false(diff$significant)
  
  # significant difference
  diff <- .compare_hdi(alpha_a = 100, beta_a = 100, alpha_b = 300, beta_b = 100)
  expect_true(diff$significant)
  
  # HDI range too wide (alpha/beta not used)
  expect_warning(diff <- .compare_hdi(alpha_a = 0.5, beta_a = 0.5, alpha_b = 10, beta_b = 10, range_a = 1, range_b = 0.7))
  expect_equal(diff$significant, NA)
  
})
