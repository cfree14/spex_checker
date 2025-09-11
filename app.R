# app.R
library(shiny)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(readr)

ui <- fluidPage(
  titlePanel("ABC Buffer Calculator"),
  
  # --- Controls and instructions ABOVE the table ---
  wellPanel(
    fluidRow(
      column(
        width = 6,
        fileInput(
          inputId = "excel",
          label   = "Upload Excel file (.xlsx or .xls)",
          accept  = c(
            ".xlsx", ".xls",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            "application/vnd.ms-excel"
          )
        )
      ),
      column(
        width = 6,
        uiOutput("sheet_picker"),
        br(),
        downloadButton("download_csv", "Download table (CSV)")
      )
    ),
    uiOutput("messages")  # explanatory text shown ABOVE the table
  ),
  
  # --- Results table BELOW ---
  DTOutput("results_table")
)

server <- function(input, output, session) {
  
  # let user pick sheet if multiple
  sheets <- reactive({
    req(input$excel)
    tryCatch(excel_sheets(input$excel$datapath), error = function(e) character(0))
  })
  
  output$sheet_picker <- renderUI({
    req(input$excel)
    s <- sheets()
    if (length(s) <= 1) return(NULL)
    selectInput("which_sheet", "Sheet:", choices = s, selected = s[[1]])
  })
  
  # read data
  data_orig <- reactive({
    req(input$excel)
    sheet_to_read <- if (!is.null(input$which_sheet)) input$which_sheet else 1
    read_excel(path = input$excel$datapath, sheet = sheet_to_read)
  })
  
  required_cols <- c(
    "stock_id", "assess_year", "year", "category", "pstar",
    "buffer", "ofl", "abc", "acl"
  )
  
  err_msg <- reactiveVal(NULL)
  
  processed <- reactive({
    req(data_orig())
    err_msg(NULL)
    
    r <- 0.075  # fixed
    
    tryCatch({
      df <- data_orig() %>% janitor::clean_names("snake")
      
      missing <- setdiff(required_cols, names(df))
      if (length(missing) > 0) {
        stop(sprintf(
          "Missing required columns after cleaning names: %s",
          paste(missing, collapse = ", ")
        ))
      }
      
      numeric_cols <- c("year", "assess_year", "category", "pstar", "buffer", "ofl", "abc", "acl")
      for (cc in numeric_cols) {
        if (!is.numeric(df[[cc]])) suppressWarnings(df[[cc]] <- as.numeric(df[[cc]]))
      }
      
      out <- df %>%
        mutate(nyr_since_assessed = year - assess_year) %>%
        mutate(
          sigma = case_when(
            category == 1 ~ 0.5,
            category == 2 ~ 1.0,
            category == 3 ~ 2.0,
            TRUE ~ NA_real_
          )
        ) %>%
        mutate(sigma_adj = sigma * (1 + r * (nyr_since_assessed - 1))) %>%
        mutate(buffer_calc = 1 - qlnorm(pstar, meanlog = 0, sdlog = sigma_adj)) %>%
        mutate(abc_calc = ofl * (1 - buffer_calc)) %>%
        mutate(buffer_diff = buffer_calc - buffer,
               abc_diff    = abc_calc - abc) %>%
        select(
          stock_id, assess_year, year, nyr_since_assessed,
          category, sigma, sigma_adj, pstar,
          buffer, buffer_calc, buffer_diff,
          ofl, abc, abc_calc, abc_diff, acl
        ) %>%
        # Round units for display
        mutate(
          buffer_calc = round(buffer_calc, 3),
          buffer_diff = round(buffer_diff, 3),
          abc_calc    = round(abc_calc, 4),
          abc_diff = round(buffer_diff, 4),
          sigma_adj = round(sigma_adj, 4),
        )
      
      out
    },
    error = function(e) {
      err_msg(e$message)
      NULL
    })
  })
  
  output$results_table <- renderDT({
    validate(
      need(!is.null(processed()), if (!is.null(err_msg())) paste("Processing error:", err_msg()) else "Upload a file to begin.")
    )
    datatable(
      processed(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 500,
        autoWidth = TRUE,
        orderClasses = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        scrollX = TRUE
      ),
      extensions = c("Buttons")
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("abc_buffer_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(processed())
      write_csv(processed(), file, na = "")
    }
  )
  
  output$messages <- renderUI({
    tagList(
      tags$p(
        style = "color:#555;",
        "Column names are cleaned to snake_case. Required columns: ",
        code(paste(required_cols, collapse = ", ")), "."
      ),
      if (!is.null(err_msg()))
        tags$p(style = "color:#b00020; font-weight:600;", paste("Processing error:", err_msg()))
    )
  })
}

shinyApp(ui, server)