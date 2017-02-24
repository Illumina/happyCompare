context("happy summary")

test_that("tidy.happy_summary works", {
    demo_happy_summary = happyCompare::demo_haplocompare_germline
    hs = demo_happy_summary$happy_summary
    ths = tidy(hs)
    
    expect_is(ths, "data.table")
    expect_equal(dim(ths)[1], 16)
    expect_equal(dim(ths)[2], 19)
})

test_that("summary.happy_summary works", {
    demo_happy_summary = happyCompare::demo_haplocompare_germline
    hs = demo_happy_summary$happy_summary
    
    s = summary(hs, type = 'SNP', aggregate = TRUE)
    expect_is(s, "knitr_kable")
    expect_equal(length(s), 4) ## expect 4 rows
    
    colnames = c("Sample", "Recall", "Precision", "Frac.NA", "#QUERY", "TP", "FN", "FP", "UNK")
    s = summary(hs, type = 'SNP', aggregate = FALSE, colnames = colnames)
    expect_equal(length(s), 6) ## expect 6 rows
    
})
