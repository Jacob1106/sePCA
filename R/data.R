# R/data.R

#' Select top variable features
#'
#' @param se A SummarizedExperiment object
#' @param n Number of top variable features to select (default: 500)
#' @param assay_name Name of assay to use (default: "counts")
#'
#' @return A SummarizedExperiment subset to the top n variable features
#' @export
#'
#' @examples
#' # Assuming 'se' is a SummarizedExperiment
#' # se_top <- top_variable_features(se, n = 500)
top_variable_features <- function(se, n = 500, assay_name = "counts") {
    mat <- SummarizedExperiment::assay(se, assay_name)
    vars <- apply(mat, 1, var)
    top_idx <- order(vars, decreasing = TRUE)[seq_len(min(n, length(vars)))]
    se[top_idx, ]
}