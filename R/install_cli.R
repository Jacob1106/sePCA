#' Install sePCA CLI launchers
#'
#' Places lightweight launcher scripts on the user's `PATH` so the
#' sePCA CLI can be invoked directly from a terminal (e.g. `sePCA pca --help`).
#'
#' @inheritDotParams Rapp::install_pkg_cli_apps -package -lib.loc
#' @export
install_sePCA_cli <- function(...) {
    Rapp::install_pkg_cli_apps(package = "sePCA", lib.loc = NULL, ...)
}