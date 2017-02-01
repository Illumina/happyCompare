context("build metrics")

test_that("demo data are loaded in the expected format", {
    setwd(path.package('happyCompare'))
    
    config_path = "data-raw/config.build_metrics.csv"
    demo_build_metrics = load_data(config_path = config_path)
    bm = demo_build_metrics$build_metrics
    
    expect_true(is_haplocompare(demo_build_metrics))
    expect_true(is_build_metrics(bm))
    
    expect_equal(dim(bm[[1]])[1], 1)
    expect_equal(dim(bm[[1]])[2], 7)
})

test_that("tidy.build_metrics works", {
    bm = demo_build_metrics$build_metrics
    
    tbm = tidy(bm, metrics = c('autosome_mean_coverage', 'percent_q30_bases'))
    expect_is(tbm, "data.table")
    expect_equal(dim(tbm)[1], 4)
    expect_equal(dim(tbm)[2], 5)
    
    expect_error(tidy(bm, metrics = c('metrics_version', 'percent_q30_bases')))
})

test_that("rename.build_metrics works", {
    bm = demo_build_metrics$build_metrics
    
    renamed_bm = rename(bm, metrics_map = metrics_map)    
    tbm = tidy(renamed_bm, metrics = c('metrics_version', '% Q30 bases'))
    expect_is(tbm, "data.table")
    expect_equal(dim(tbm)[1], 4)
    expect_equal(dim(tbm)[2], 5)
})