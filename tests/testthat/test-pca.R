# tests/testthat/test-pca.R

test_that("run_pca returns correct structure", {
    data(example_se, package = "sePCA")
    
    result <- run_pca(example_se, n_top = 50)
    
    expect_type(result, "list")
    expect_named(result, c("pca", "scores"))
    expect_s3_class(result$pca, "prcomp")
    expect_s3_class(result$scores, "data.frame")
})

test_that("run_pca scores contain sample metadata", {
    data(example_se, package = "sePCA")
    
    result <- run_pca(example_se, n_top = 50)
    
    # Should have treatment column from colData
    expect_true("treatment" %in% colnames(result$scores))
    expect_true("sample_id" %in% colnames(result$scores))
})

test_that("run_pca returns expected number of PCs", {
    data(example_se, package = "sePCA")
    
    result <- run_pca(example_se, n_top = 50)
    
    # Should have PCs equal to min(n_samples, n_features)
    n_samples <- ncol(example_se)
    expect_true(paste0("PC", 1) %in% colnames(result$scores))
    expect_true(paste0("PC", n_samples) %in% colnames(result$scores))
})

test_that("pca_variance_explained returns percentages", {
    data(example_se, package = "sePCA")
    
    result <- run_pca(example_se, n_top = 50)
    var_df <- pca_variance_explained(result)
    
    # Variance should sum to 100
    expect_equal(sum(var_df$variance_percent), 100, tolerance = 0.01)
    
    # Should be sorted descending (PC1 explains most)
    expect_true(var_df$variance_percent[1] >= var_df$variance_percent[2])
})