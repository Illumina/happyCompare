context("happy roc")

test_that("plot.happy_roc works", {
  roc = hapdata$pr_curve$all

  p1 = plot(roc, type = "SNP", filter = "ALL")  
  p2 = plot(roc, type = "SNP", filter = "PASS")
  p3 = plot(roc, type = "SNP", filter = "SEL")

  p4 = plot(roc, type = "INDEL", filter = "ALL")  
  p5 = plot(roc, type = "INDEL", filter = "PASS")
  p6 = plot(roc, type = "INDEL", filter = "SEL")
  
  invisible(suppressWarnings(file.remove("test_happy_roc.pdf")))
  
  pdf(file = "test_happy_roc.pdf", width = 12, height = 4)
  gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
  gridExtra::grid.arrange(p4, p5, p6, nrow = 1)
  dev.off()

  expect_true(file.exists("test_happy_roc.pdf"))
})
