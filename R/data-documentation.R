#' Example SummarizedExperiment for testing
#'
#' A small SummarizedExperiment with 100 genes and 8 samples.
#' Includes a treatment effect in the first 20 genes.
#'
#' @format A SummarizedExperiment with:
#' \describe{
#'   \item{assays}{counts - raw count matrix}
#'   \item{colData}{sample_id, treatment (control/treated), batch (A/B)}
#'   \item{rowData}{gene_id, gene_symbol}
#' }
#'
#' @source Simulated data for teaching purposes
#'@importFrom SummarizedExperiment colData
#' @examples
#' data(example_se)
#' example_se
#' SummarizedExperiment::colData(example_se)
"example_se"