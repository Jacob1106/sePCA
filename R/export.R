# R/export.R

#' Save PCA results to files
#'
#' @param pca_result Output from run_pca()
#' @param output_dir Directory to save files
#' @param prefix Prefix for filenames (default: "pca")
#'
#' @return Invisible NULL; files are written to output_dir
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_pca(se)
#' save_pca_results(result, "output/")
#' }
save_pca_results <- function(pca_result, output_dir, prefix = "pca") {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    # Save scores
    scores_file <- file.path(output_dir, paste0(prefix, "_scores.tsv"))
    utils::write.table(
        pca_result$scores,
        scores_file,
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
    )
    
    # Save variance explained
    var_file <- file.path(output_dir, paste0(prefix, "_variance.tsv"))
    var_df <- pca_variance_explained(pca_result)
    utils::write.table(
        var_df,
        var_file,
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
    )
    
    message("Saved: ", scores_file)
    message("Saved: ", var_file)
    
    invisible(NULL)
}