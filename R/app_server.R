# R/app_server.R

#' Shiny App Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return NULL (side effects only)
#' @noRd
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom ggplot2 ggplot aes geom_col geom_text theme_minimal labs ylim
#' @importFrom rlang .data
app_server <- function(input, output, session) {
    # Load example data
    # In a real app, you might want to let users upload their own data
    se_data <- shiny::reactive({
        data("airway", package = "airway", envir = environment())
        get("airway", envir = environment())
    })
    
    # Update select inputs based on available metadata
    shiny::observe({
        se <- se_data()
        cols <- colnames(SummarizedExperiment::colData(se))
        shiny::updateSelectInput(session, "color_by", choices = cols)
        shiny::updateSelectInput(session, "shape_by", choices = c("None", cols))
    })
    
    # Compute PCA
    pca_result <- shiny::reactive({
        shiny::req(se_data(), input$n_top)
        
        run_pca(
            se_data(),
            n_top = input$n_top,
            log_transform = input$log_transform,
            scale = input$scale
        )
    })
    
    # PCA scatter plot
    output$pca_plot <- shiny::renderPlot({
        shiny::req(pca_result(), input$color_by)
        
        shape <- if (is.null(input$shape_by) || input$shape_by == "None") {
            NULL
        } else {
            input$shape_by
        }
        
        plot_pca(
            pca_result(),
            color_by = input$color_by,
            shape_by = shape,
            point_size = input$point_size
        )
    })
    
    # Variance plot
    output$variance_plot <- shiny::renderPlot({
        shiny::req(pca_result())
        
        var_df <- pca_variance_explained(pca_result())
        var_df <- var_df[seq_len(min(8, nrow(var_df))), ]
        var_df$PC <- factor(var_df$PC, levels = var_df$PC)
        
        ggplot2::ggplot(var_df, ggplot2::aes(x = .data$PC, y = .data$variance_percent)) +
            ggplot2::geom_col(fill = "steelblue") +
            ggplot2::geom_text(
                ggplot2::aes(label = sprintf("%.1f%%", .data$variance_percent)),
                vjust = -0.5, size = 4
            ) +
            ggplot2::theme_minimal(base_size = 14) +
            ggplot2::labs(
                x = "Principal Component",
                y = "Variance Explained (%)"
            ) +
            ggplot2::ylim(0, max(var_df$variance_percent) * 1.15)
    })
    
    # Scores table
    output$scores_table <- DT::renderDataTable({
        shiny::req(pca_result())
        DT::datatable(
            pca_result()$scores,
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    # Download handler
    output$download_plot <- shiny::downloadHandler(
        filename = function() {
            paste0("pca_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            shape <- if (is.null(input$shape_by) || input$shape_by == "None") {
                NULL
            } else {
                input$shape_by
            }
            
            p <- plot_pca(
                pca_result(),
                color_by = input$color_by,
                shape_by = shape,
                point_size = input$point_size
            )
            
            ggplot2::ggsave(file, p, width = 8, height = 6, dpi = 150)
        }
    )
}