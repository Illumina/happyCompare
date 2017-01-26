context("happy summary")

test_that("demo data are loaded in the expected format", {
    setwd(path.package('happyCompare'))
    
    config_path = "data-raw/config.happy_summary.csv"
    demo_happy_summary = load_data(config_path = config_path)
    hs = demo_happy_summary$happy_summary
    
    expect_true(is_happy_compare(demo_happy_summary))
    expect_true(is_happy_summary(hs))
    
    expect_equal(dim(hs[[1]])[1], 4)
    expect_equal(dim(hs[[1]])[2], 19)
})

test_that("tidy.happy_summary works", {
    hs = demo_happy_summary$happy_summary
    ths = tidy(hs)
    expect_is(ths, "data.frame")
    expect_equal(dim(ths)[1], 16)
    expect_equal(dim(ths)[2], 20)
})

test_that("plot.happy_summary works", {
    hs = demo_happy_summary$happy_summary
    p = plot(hs, type = 'SNP')
    expect_is(p, "gtable")
})

test_that("summary.happy_summary works", {
    hs = demo_happy_summary$happy_summary
    s = summary(hs, type = 'SNP')
    expect_is(s, "knitr_kable")
})
