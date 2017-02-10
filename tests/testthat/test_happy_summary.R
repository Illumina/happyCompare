context("happy summary")

test_that("demo data are loaded in the expected format", {
    demo_happy_summary = happyCompare::demo_haplocompare_germline
    hs = demo_happy_summary$happy_summary
    
    expect_true(is_haplocompare(demo_happy_summary))
    expect_true(is_happy_summary(hs))
    
    expect_equal(dim(hs[[1]])[1], 4)
    expect_equal(dim(hs[[1]])[2], 19)
})

test_that("tidy.happy_summary works", {
    demo_happy_summary = happyCompare::demo_haplocompare_germline
    hs = demo_happy_summary$happy_summary
    ths = tidy(hs)
    
    expect_is(ths, "data.table")
    expect_equal(dim(ths)[1], 16)
    expect_equal(dim(ths)[2], 19)
})

test_that("plot.happy_summary works", {
    demo_happy_summary = happyCompare::demo_haplocompare_germline
    hs = demo_happy_summary$happy_summary
    p = plot(hs, type = 'SNP')
    expect_is(p, "gtable")
})

test_that("summary.happy_summary works", {
    demo_happy_summary = happyCompare::demo_haplocompare_germline
    hs = demo_happy_summary$happy_summary
    s = summary(hs, type = 'SNP')
    expect_is(s, "knitr_kable")
})
