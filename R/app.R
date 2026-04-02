ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    
    titlePanel("PCA Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            h4("PCA Settings"),
            numericInput(
                "n_top",
                "Top variable genes:",
                value = 500, min = 50, max = 5000, step = 50
            ),
            checkboxInput(
                "log_transform",
                "Log-transform counts",
                value = TRUE
            ),
            checkboxInput(
                "scale",
                "Scale features",
                value = TRUE
            ),
            
            hr(),
            h4("Plot Settings"),
            
            selectInput("color_by", "Color by:", choices = c("dex", "cell")),
            selectInput("shape_by", "Shape by:", choices = c("None", "dex", "cell")),
            
            fluidRow(
                column(6, numericInput("pc_x", "PC X:", value = 1, min = 1, max = 8)),
                column(6, numericInput("pc_y", "PC Y:", value = 2, min = 1, max = 8))
            ),
            
            sliderInput(
                "point_size",
                "Point size:",
                value = 4, min = 1, max = 10
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "PCA Plot",
                    plotOutput("pca_plot", height = "500px")
                ),
                tabPanel(
                    "Variance Explained",
                    plotOutput("variance_plot", height = "400px")
                ),
                tabPanel(
                    "Sample Data",
                    DT::dataTableOutput("scores_table")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Cached PCA result
    pca_result <- reactive({
        run_pca(
            airway,
            n_top = input$n_top,
            log_transform = input$log_transform,
            scale = input$scale
        )
    })
    
    # PCA scatter plot
    output$pca_plot <- renderPlot({
        shape <- if (input$shape_by == "None") NULL else input$shape_by
        
        plot_pca(
            pca_result(),
            color_by = input$color_by,
            shape_by = shape,
            pcs = c(input$pc_x, input$pc_y),
            point_size = input$point_size
        )
    })
    
    # Variance plot
    output$variance_plot <- renderPlot({
        var_df <- pca_variance_explained(pca_result())
        
        # Only show first 8 PCs
        var_df <- var_df[1:min(8, nrow(var_df)), ]
        var_df$PC <- factor(var_df$PC, levels = var_df$PC)
        
        ggplot2::ggplot(var_df, ggplot2::aes(x = PC, y = variance_percent)) +
            ggplot2::geom_col(fill = "steelblue") +
            ggplot2::geom_text(
                ggplot2::aes(label = sprintf("%.1f%%", variance_percent)),
                vjust = -0.5
            ) +
            ggplot2::theme_minimal(base_size = 14) +
            ggplot2::labs(
                x = "Principal Component",
                y = "Variance Explained (%)",
                title = "Variance Explained by Each PC"
            ) +
            ggplot2::ylim(0, max(var_df$variance_percent) * 1.1)
    })
    
    # Scores table
    output$scores_table <- DT::renderDataTable({
        pca_result()$scores
    }, options = list(pageLength = 10, scrollX = TRUE))
}

shinyApp(ui, server)