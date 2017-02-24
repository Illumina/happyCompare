context("haplocompare")

test_that("demo data are loaded in the expected format", {
    demo_haplocompare = happyCompare::demo_haplocompare_germline
    
    expect_true(is_haplocompare(demo_haplocompare))
    expect_equal(sum(!sapply(demo_haplocompare, is.null)), 4)
})