context("reporting.R")

samplesheet_path <- system.file("extdata/samplesheets", "pcrfree_vs_nano.tests.csv", package = "happyCompare")
happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE)

test_that("hc_summarise_metrics works", {
  
  e <- extract_metrics(happy_compare, table = "summary")
  expect_is(e, "happy_summary")
  
  t <- hc_summarise_metrics(df = e)
  expect_is(t, "data.frame")
  expect_equal(dim(t)[1], 8)
  expect_equal(dim(t)[2], 7)

})

test_that("hc_plot_roc works", {
  
  roc <- extract_metrics(happy_compare, table = "pr.all")
  pass <- hc_plot_roc(roc, type = "INDEL", filter = "PASS")
  sel <- hc_plot_roc(roc, type = "INDEL", filter = "SEL")
  all <- hc_plot_roc(roc, type = "INDEL", filter = "ALL")
  
  OUTFILE <- "test.hc_plot_roc.pdf"
  invisible(file.remove(OUTFILE))
  pdf(file = OUTFILE, width = 12, height = 4)
  gridExtra::grid.arrange(pass, sel, all, nrow = 1)
  dev.off()
  
  expect_true(file.exists(OUTFILE))
  expect_true(file.info(OUTFILE)$size > 0)
  
})

test_that("hc_plot_hdi works", {
  
  d <- extract_metrics(happy_compare, table = "extended") %>% 
    dplyr::filter(Type == "INDEL", Subtype == "*", Filter == "PASS", 
                  Subset.Level == 0, !grepl(pattern = "TS*", Subset))
  expect_is(d, "data.frame")
  expect_equal(dim(d)[1], 20)
  expect_equal(dim(d)[2], 69)
  
  successes_col <- "TRUTH.TP"
  totals_col <- "TRUTH.TOTAL"
  group_cols <- c("Group.Id", "Subset", "Type")
  e <- estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
                     group_cols = group_cols, aggregate_only = FALSE, sample_size = 1000)
  
  h <- e %>% dplyr::filter(Group.Id == "PCR-Free", Subset == "high.at")
  pcrfree <- hc_plot_hdi(h, title = "PCR-Free high.at")
  h <- e %>% dplyr::filter(Group.Id == "Nano", Subset == "high.at")
  nano <- hc_plot_hdi(h, title = "Nano high.at")
  h <- e %>% dplyr::filter(Subset == "high.at")
  both <- hc_plot_hdi(h, title = "PCR-Free vs Nano high.at")

  OUTFILE <- "test.hc_plot_hdi.pdf"
  invisible(file.remove(OUTFILE))
  pdf(file = OUTFILE, width = 12, height = 4)
  gridExtra::grid.arrange(pcrfree, nano, both, nrow = 1)
  dev.off()
  
  expect_true(file.exists(OUTFILE))
  expect_true(file.info(OUTFILE)$size > 0)
  
})
