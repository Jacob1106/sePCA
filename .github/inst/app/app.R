# # inst/app/app.R

# # Launch the sePCA Shiny application
# # This file exists for compatibility with runApp()
# # Preferred method: sePCA::run_app()

# if (!requireNamespace("sePCA", quietly = TRUE)) {
#     stop("Please install sePCA first: remotes::install_github('Jacob1106/sePCA')")
# }

# sePCA:::app_ui
# sePCA:::app_server

# shiny::shinyApp(
#     ui = sePCA:::app_ui(),
#     server = sePCA:::app_server
# )