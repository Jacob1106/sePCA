# tests/testthat/test-export.R

test_that("save_pca_results creates files", {
    data(example_se, package = "sePCA")
    result <- run_pca(example_se, n_top = 50)
    
    # Use a temporary directory
    tmp_dir <- tempdir()
    output_dir <- file.path(tmp_dir, "test_output")
    
    save_pca_results(result, output_dir, prefix = "test")
    
    # Check files exist
    expect_true(file.exists(file.path(output_dir, "test_scores.tsv")))
    expect_true(file.exists(file.path(output_dir, "test_variance.tsv")))
    
    # Check scores file has correct structure
    scores <- read.table(
        file.path(output_dir, "test_scores.tsv"),
        header = TRUE,
        sep = "\t"
    )
    expect_true("PC1" %in% colnames(scores))
    expect_true("sample_id" %in% colnames(scores))
    
    # Clean up
    unlink(output_dir, recursive = TRUE)
})