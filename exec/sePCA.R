#!/usr/bin/env Rapp
#| name: sePCA
#| title: sePCA CLI
#| description: PCA analysis for SummarizedExperiment data.

suppressPackageStartupMessages(library(sePCA))

# Helper to read TSV/CSV
read_data_file <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "csv") {
        read.csv(path, row.names = 1, check.names = FALSE)
    } else {
        read.table(path, sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)
    }
}

switch(
    "",

    #| title: Run PCA analysis
    #| description: Run PCA and export results.
    pca = {
        #| description: Path to counts matrix (TSV/CSV)
        #| short: c
        counts <- ""

        #| description: Path to sample metadata (TSV/CSV)
        #| short: m
        meta <- ""

        #| description: Output directory
        #| short: o
        output <- ""

        #| description: Number of top variable genes
        #| short: n
        n_top <- 500L

        #| description: Log-transform counts
        log_transform <- TRUE

        #| description: Metadata column for plot coloring (optional)
        color_by <- ""

        if (counts == "" || meta == "" || output == "") {
            stop("--counts, --meta, and --output are required", call. = FALSE)
        }
        if (!file.exists(counts)) stop("File not found: ", counts, call. = FALSE)
        if (!file.exists(meta)) stop("File not found: ", meta, call. = FALSE)

        if (!dir.exists(output)) dir.create(output, recursive = TRUE)

        counts_df <- read_data_file(counts)
        meta_df <- read_data_file(meta)
        se <- SummarizedExperiment::SummarizedExperiment(
            assays = list(counts = as.matrix(counts_df)),
            colData = meta_df
        )

        result <- run_pca(se, n_top = n_top, log_transform = log_transform)

        scores_file <- file.path(output, "pca_scores.tsv")
        write.table(result$scores, scores_file, sep = "\t", row.names = FALSE, quote = FALSE)

        var_file <- file.path(output, "pca_variance.tsv")
        var_df <- pca_variance_explained(result)
        write.table(var_df, var_file, sep = "\t", row.names = FALSE, quote = FALSE)

        if (color_by != "") {
            plot_file <- file.path(output, "pca_plot.png")
            p <- plot_pca(result, color_by = color_by)
            ggplot2::ggsave(plot_file, p, width = 8, height = 6, dpi = 150)
        }
    },

    #| title: Validate input files
    #| description: Check that inputs exist, parse, and report dimensions.
    validate = {
        #| description: Path to counts matrix (TSV/CSV)
        counts <- ""

        #| description: Path to sample metadata (TSV/CSV)
        meta <- ""

        if (counts == "" || meta == "") {
            stop("--counts and --meta are required", call. = FALSE)
        }
        if (!file.exists(counts)) stop("File not found: ", counts, call. = FALSE)
        if (!file.exists(meta)) stop("File not found: ", meta, call. = FALSE)

        counts_df <- read_data_file(counts)
        meta_df <- read_data_file(meta)

        message("Counts dimensions: ", nrow(counts_df), " genes x ", ncol(counts_df), " samples")
        message("Metadata rows: ", nrow(meta_df))

        if (!all(colnames(counts_df) %in% rownames(meta_df))) {
            stop("Sample IDs in counts do not match metadata row names", call. = FALSE)
        }

        message("Inputs look valid.")
    }
)
