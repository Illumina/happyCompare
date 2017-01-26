context("happy extended ci")

test_that("demo data are loaded in the expected format", {
    he_ci = demo_happy_extended_ci
    
    expect_true(is_happy_extended_ci(he_ci))
    
    expect_equal(dim(he_ci)[1], 6126)
    expect_equal(dim(he_ci)[2], 15)
})

test_that("plot_subset.happy_extended_ci works", {
    p = plot_subset(demo_happy_extended_ci, subset = 'hgnc_genes_ANK3', type = 'SNP', metric = 'METRIC.Recall')
    expect_is(p, "gtable")
})
