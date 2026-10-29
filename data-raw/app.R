
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# pca_scores: series_id, pattern, PC1, PC2
# series_list: list of numeric time series

# Match each point to its series using row order
pca_scores <- as.data.frame(pca_scores)

if (!"series_index" %in% names(pca_scores)) {
  pca_scores$series_index <- seq_len(nrow(pca_scores))
}

ui <- fluidPage(
  titlePanel("Explore Time Series in PCA Space"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "colour_by",
        "Colour points by",
        choices = c(
          "Pattern" = "pattern",
          "Series ID" = "series_id"
        )
      ),
      
      checkboxInput(
        "show_labels",
        "Show series ID labels",
        FALSE
      ),
      
      hr(),
      h4("Selected time series"),
      verbatimTextOutput("selected_info")
    ),
    
    mainPanel(
      plotlyOutput("pca_plot", height = "500px"),
      plotOutput("series_plot", height = "350px")
    )
  )
)

server <- function(input, output, session) {
  
  selected_row <- reactiveVal(1L)
  
  output$pca_plot <- renderPlotly({
    
    dat <- pca_scores
    dat$.row <- seq_len(nrow(dat))
    
    p <- ggplot(
      dat,
      aes(
        x = PC1,
        y = PC2,
        colour = .data[[input$colour_by]],
        key = .row,
        text = paste0(
          "Series: ", series_id,
          "<br>Pattern: ", pattern,
          "<br>PC1: ", round(PC1, 3),
          "<br>PC2: ", round(PC2, 3)
        )
      )
    ) +
      geom_point(size = 3, alpha = 0.85) +
      labs(
        x = "PC1",
        y = "PC2",
        colour = input$colour_by
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if (input$show_labels) {
      p <- p +
        geom_text(
          aes(label = series_id),
          vjust = -0.7,
          size = 3,
          show.legend = FALSE
        )
    }
    
    ggplotly(p, tooltip = "text", source = "pca")
  })
  
  observeEvent(
    event_data("plotly_click", source = "pca"),
    {
      click <- event_data("plotly_click", source = "pca")
      
      if (!is.null(click$key)) {
        selected_row(as.integer(click$key[[1]]))
      }
    }
  )
  
  selected_data <- reactive({
    pca_scores[selected_row(), , drop = FALSE]
  })
  
  output$selected_info <- renderPrint({
    row <- selected_data()
    
    cat("Series ID:", row$series_id, "\n")
    cat("Pattern:", row$pattern, "\n")
    cat("PC1:", round(row$PC1, 4), "\n")
    cat("PC2:", round(row$PC2, 4), "\n")
    cat(
      "Length:",
      length(series_list[[row$series_index]])
    )
  })
  
  output$series_plot <- renderPlot({
    
    row <- selected_data()
    y <- series_list[[row$series_index]]
    
    validate(
      need(length(y) > 0, "No observations in this series.")
    )
    
    series_df <- data.frame(
      Time = seq_along(y),
      Value = as.numeric(y)
    )
    
    ggplot(series_df, aes(Time, Value)) +
      geom_line(linewidth = 0.7, colour = "#2878B5") +
      labs(
        title = paste(
          "Time series:",
          row$series_id,
          "—",
          row$pattern
        ),
        x = "Time index",
        y = "Value"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

