context("reporting.R")

test_that("tabulate.happy_summary works", {
  s <- extract(happy_compare, table = "summary")
  expect_true("happy_summary" %in% class(s))
  
  cols <- c("Group.Id", "Replicate.Id", "METRIC.Recall", "METRIC.Precision", "METRIC.Frac_NA", 
    "METRIC.F1_Score", "QUERY.TOTAL", "TRUTH.TP", "TRUTH.FN", "QUERY.FP", "QUERY.UNK")
  colnames <- c("Group.Id", "Sample", "Recall", "Precision", "Frac_NA", "F1_Score", 
    "QUERY.TOTAL", "TP", "FN", "FP", "UNK")
  
  t <- tabulate(happy_summary = s, cols = cols, colnames = colnames, filter = "PASS", 
    vartype = "SNP", aggregate = FALSE)
  expect_is(t, "tibble")
  # TODO: add test for dim
  
  t <- tabulate(happy_summary = s, cols = cols, colnames = colnames, filter = "PASS", 
    vartype = "SNP", aggregate = TRUE)
  expect_is(t, "tibble")
  # TODO: add test for dim
})

test_that("plot.happy_roc works", {
  roc <- extract(happy_compare, table = "pr_curve.all")
  expect_true("happy_roc" %in% class(roc))
  
  p1 <- plot(roc, type = "SNP", filter = "ALL")
  p2 <- plot(roc, type = "SNP", filter = "PASS")
  p3 <- plot(roc, type = "SNP", filter = "SEL")
  
  p4 <- plot(roc, type = "INDEL", filter = "ALL")
  p5 <- plot(roc, type = "INDEL", filter = "PASS")
  p6 <- plot(roc, type = "INDEL", filter = "SEL")
  
  OUTFILE <- "test_plot.happy_roc.pdf"
  invisible(file.remove(OUTFILE))
  
  ggplot2::theme_set(theme_bw())
  pdf(file = OUTFILE, width = 12, height = 4)
  gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
  gridExtra::grid.arrange(p4, p5, p6, nrow = 1)
  dev.off()
  
  expect_true(file.exists(OUTFILE))
  expect_true(file.info(OUTFILE)$size > 0)
})

test_that("plot.happy_hdi works", {
  d <- happy_extended
  successes_col <- "TRUTH.TP"
  totals_col <- "TRUTH.TOTAL"
  group_cols <- c("Group.Id", "Subset", "Type", "Subtype")
  
  e1 <- estimate_hdi(happy_extended = d, successes_col = successes_col, totals_col = totals_col, 
    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000)
  e2 <- estimate_hdi(happy_extended = d, successes_col = successes_col, totals_col = totals_col, 
    group_cols = group_cols, aggregate_only = FALSE, sample_size = 1000)
  
  s <- e1 %>% filter(Subset == "g.rich", Group.Id == "PCR-Free", Type == "INDEL", 
    Subtype == "D1_5")
  p1 <- plot(happy_hdi = s, title = "PCR-Free g.rich INDEL D1_5 - aggregate HDI only")
  
  s <- e2 %>% filter(Subset == "g.rich", Group.Id == "PCR-Free", Type == "INDEL", 
    Subtype == "D1_5")
  p2 <- plot(happy_hdi = s, title = "PCR-Free g.rich INDEL D1_5")
  
  s <- e1 %>% filter(Subset == "g.rich", Type == "INDEL", Subtype == "D1_5")
  p3 <- plot(happy_hdi = s, title = "g.rich INDEL D1_5 - two groups")
  
  OUTFILE <- "test_plot.happy_hdi.pdf"
  invisible(suppressWarnings(file.remove(OUTFILE)))
  
  ggplot2::theme_set(theme_bw())
  pdf(file = OUTFILE, width = 5, height = 10)
  gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
  dev.off()
  
  expect_true(file.exists(OUTFILE))
})
