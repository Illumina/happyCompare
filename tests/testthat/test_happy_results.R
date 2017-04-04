context("happy_results")


test_that("tabulate.happy_summary works", {
  s = extract(happyCompare_list, table = "summary")
  expect_true("happy_summary" %in% class(s))
  
  cols = c("Group.Id", "Replicate.Id", 
           "METRIC.Recall", "METRIC.Precision", "METRIC.Frac_NA", "METRIC.F1_Score",
           "QUERY.TOTAL", "TRUTH.TP", "TRUTH.FN", "QUERY.FP", "QUERY.UNK")
  colnames = c("Group.Id", "Sample", 
               "Recall", "Precision", "Frac_NA", "F1_Score",
               "QUERY.TOTAL", "TP", "FN", "FP", "UNK")
  
  t = tabulate(happy_summary = s, cols = cols, colnames = colnames, 
               filter = "PASS", vartype = "SNP", aggregate = FALSE)
  expect_true("data.frame" %in% class(t))
  
  t = tabulate(happy_summary = s, cols = cols, colnames = colnames, 
               filter = "PASS", vartype = "SNP", aggregate = TRUE)
  expect_true("data.frame" %in% class(t))
})


test_that("plot.happy_roc works", {
  roc = extract(happyCompare_list, table = "pr_curve.all")
  expect_true("happy_roc" %in% class(roc))
  
  p1 = plot(roc, type = "SNP", filter = "ALL")  
  p2 = plot(roc, type = "SNP", filter = "PASS")
  p3 = plot(roc, type = "SNP", filter = "SEL")
  
  p4 = plot(roc, type = "INDEL", filter = "ALL")  
  p5 = plot(roc, type = "INDEL", filter = "PASS")
  p6 = plot(roc, type = "INDEL", filter = "SEL")
  
  invisible(suppressWarnings(file.remove("test_happy_roc.pdf")))
  
  ggplot2::theme_set(theme_bw())
  pdf(file = "test_plot.happy_roc.pdf", width = 12, height = 4)
  gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
  gridExtra::grid.arrange(p4, p5, p6, nrow = 1)
  dev.off()
  
  expect_true(file.exists("test_plot.happy_roc.pdf"))
})