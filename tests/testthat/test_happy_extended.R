context("happy extended")

test_that("demo data are loaded in the expected format", {
    setwd(path.package('happyCompare'))
    
    config_path = "data-raw/config.happy_extended.csv"
    demo_happy_extended = load_data(config_path = config_path)
    he = demo_happy_extended$happy_extended
    
    expect_true(is_happy_compare(demo_happy_extended))
    expect_true(is_happy_extended(he))
    
    expect_equal(dim(he[[1]])[1], 999)
    expect_equal(dim(he[[1]])[2], 73)
})

test_that("tidy.happy_extended works", {
    he = demo_happy_extended$happy_extended
    the = tidy(he)
    
    expect_is(the, "data.frame")
    
    expect_equal(dim(the)[1], 3996)
    expect_equal(dim(the)[2], 74)
})

test_that("add_credible_intervals.happy_extended works", {
    he = demo_happy_extended$happy_extended
    he_ci = add_credible_intervals(he, metric = 'METRIC.Recall', samplesize = 1e4)
    
    expect_true(is_happy_extended_ci(he_ci))
    
    expect_equal(dim(he_ci)[1], 6126)
    expect_equal(dim(he_ci)[2], 15)
})