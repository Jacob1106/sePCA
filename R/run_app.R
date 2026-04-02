# R/run_app.R

#' Run the sePCA Shiny Application
#'
#' Launches an interactive Shiny application for exploring PCA results
#' on SummarizedExperiment data.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()]
#'
#' @return A Shiny app object (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function(...) {
    # Check for required packages
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package 'shiny' is required. Install with: install.packages('shiny')")
    }
    if (!requireNamespace("bslib", quietly = TRUE)) {
        stop("Package 'bslib' is required. Install with: install.packages('bslib')")
    }
    if (!requireNamespace("DT", quietly = TRUE)) {
        stop("Package 'DT' is required. Install with: install.packages('DT')")
    }
    if (!requireNamespace("airway", quietly = TRUE)) {
        stop("Package 'airway' is required. Install with: BiocManager::install('airway')")
    }
    
    app <- shiny::shinyApp(
        ui = app_ui(),
        server = app_server,
        ...
    )
    
    shiny::runApp(app)
}