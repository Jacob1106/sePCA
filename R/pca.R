#' Run PCA on a SummarizedExperiment
#'
#' Selects the top variable features from a SummarizedExperiment,
#' performs a PCA on the transposed assay matrix (samples as rows),
#' and returns both the PCA object and a score table joined with
#' sample-level metadata.
#'
#' @author Jacob Martin
#'
#' @param se A \code{SummarizedExperiment} object containing the assay data
#'   and sample-level metadata.
#' @param assay_name A \code{character} scalar giving the name of the assay
#'   to use (e.g. \code{"counts"}).
#' @param n_top An \code{integer} giving the number of most variable features
#'   to retain before PCA.
#' @param scale A \code{logical} indicating whether variables should be
#'   scaled to unit variance before PCA.
#' @param log_transform A \code{logical} indicating whether to apply a
#'   \eqn{\log_2(x + 1)} transform to the assay matrix prior to PCA.
#'
#' @details
#' This function first calls \code{top_variable_features()} to subset the
#' SummarizedExperiment to the \code{n_top} most variable rows in the
#' specified assay. The selected assay is optionally log-transformed,
#' transposed so that samples are rows and features are columns, and then
#' passed to \code{stats::prcomp()} with centering and optional scaling.
#' The resulting PCA scores are merged with \code{colData(se)} by
#' \code{sample_id}, which is assumed to correspond to row names in both
#' the PCA score matrix and the sample metadata.
#'
#' @return A \code{list} with the following components:
#' \describe{
#'   \item{pca}{The \code{prcomp} object returned by \code{stats::prcomp()}.}
#'   \item{scores}{A \code{data.frame} of PCA scores for each sample,
#'     including principal component coordinates and columns from
#'     \code{colData(se)}.}
#' }
#'
#' @seealso \code{\link[SummarizedExperiment]{SummarizedExperiment}},
#'   \code{\link[SummarizedExperiment]{assay}},
#'   \code{\link[SummarizedExperiment]{colData}},
#'   \code{\link[stats]{prcomp}}
#'
#' @importFrom SummarizedExperiment assay colData
#' @importFrom stats prcomp
#' @examples
#' # Using the example SummarizedExperiment:
#' # data(example_se)
#' # res <- run_pca(example_se, assay_name = "counts", n_top = 500)
#' # str(res$pca)
#' # head(res$scores)
#'
#' @export
run_pca <- function(se, assay_name = "counts", n_top = 500, 
                    scale = TRUE, log_transform = TRUE) {
    se_top <- top_variable_features(se, n = n_top, assay_name = assay_name)
    mat <- SummarizedExperiment::assay(se_top, assay_name)
    if (log_transform) mat <- log2(mat + 1)
    mat_t <- t(mat)
    pca_result <- prcomp(mat_t, scale. = scale, center = TRUE)
    scores <- as.data.frame(pca_result$x)
    scores$sample_id <- rownames(scores)
    col_data <- as.data.frame(colData(se))
    col_data$sample_id <- rownames(col_data)
    scores <- merge(scores, col_data, by = "sample_id")
    list(pca = pca_result, scores = scores)
}

#' Get variance explained by each PC
#'
#' Calculates the percentage of total variance explained by each principal
#' component from a PCA result object.
#'
#' @param pca_result A list containing a \code{prcomp} object in the
#'   \code{pca} element, as returned by \code{\link{run_pca}}.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{PC}{Principal component name (e.g. \code{"PC1"}, \code{"PC2"})}
#'   \item{variance_percent}{Percentage of total variance explained}
#' }
#'
#' @examples
#' # res <- run_pca(example_se)
#' # var_exp <- pca_variance_explained(res)
#' # head(var_exp)
#'
#' @export
pca_variance_explained <- function(pca_result) {
    pca <- pca_result$pca
    var_explained <- pca$sdev^2 / sum(pca$sdev^2) * 100
    data.frame(PC = paste0("PC", seq_along(var_explained)), 
               variance_percent = var_explained)
}

#' Create a PCA scatter plot
#'
#' Generates a ggplot2 scatter plot of PCA scores with optional coloring
#' and shape aesthetics.
#'
#' @param pca_result A list containing PCA scores in the \code{scores}
#'   element, as returned by \code{\link{run_pca}}.
#' @param color_by A \code{character} scalar naming a column in
#'   \code{pca_result$scores} to color points by. If \code{NULL}, uses default
#'   black points.
#' @param shape_by A \code{character} scalar naming a column in
#'   \code{pca_result$scores} to set point shapes by. If \code{NULL}, uses
#'   circles.
#' @param pcs A \code{numeric} vector of length 2 giving the principal
#'   components to plot on x and y axes (default: \code{c(1, 2)}).
#' @param point_size A \code{numeric} scalar giving point size in ggplot units
#'   (default: \code{4}).
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs .data
#' @return A \code{ggplot} object.
#'
#' @examples
#' # res <- run_pca(example_se, color_by = "treatment")
#' # p <- plot_pca(res, color_by = "treatment", shape_by = "batch")
#' # print(p)
#'
#' @export
plot_pca <- function(pca_result, color_by = NULL, shape_by = NULL, 
                     pcs = c(1, 2), point_size = 4) {
    scores <- pca_result$scores
    var_exp <- pca_variance_explained(pca_result)
    pc_x <- paste0("PC", pcs[1])
    pc_y <- paste0("PC", pcs[2])
    var_x <- round(var_exp$variance_percent[pcs[1]], 1)
    var_y <- round(var_exp$variance_percent[pcs[2]], 1)
    p <- ggplot(scores, aes(x = .data[[pc_x]], y = .data[[pc_y]])) +
        theme_minimal(base_size = 14) +
        labs(x = paste0(pc_x, " (", var_x, "% variance)"),
             y = paste0(pc_y, " (", var_y, "% variance)"),
             title = "PCA Plot")
    if (!is.null(color_by)) p <- p + aes(color = .data[[color_by]])
    if (!is.null(shape_by)) p <- p + aes(shape = .data[[shape_by]])
    p + geom_point(size = point_size)
}
