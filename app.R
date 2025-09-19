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
  
  # Explanatory text
  wellPanel(
    h3("App description"),
    p("The size of the buffer between the OFL and ABC is determined based on the Council's risk tolerance choice (P*, Pstar) and the SSC's assessment of scientific uncertainty (σ, sigma). The base scientific uncertainty (in the year of the assessment) is usually equal to the default value associated with the assessment's data-richness category (Category 1=0.5, Category 2=1.0, Category 3=2.0), though values different from the defaults can be used by the SSC."),
    p("The magnitude of the scientific uncertainty is assumed to increase with assessment age. The slope of this increase is 0.075/year when natural mortality (M) is less than 0.15. When natural mortality is greater than 0.15, the slope is calculated as 0.52 times the natural mortality. If natural mortality is sex-specific, the geometric mean of the male and female mortality rates is used."),
    p("As a result, the derivation of the ABC buffer requires four values: (1) Pstar, (2) the base sigma, (3) the age of the assessment, and (4) the natural mortality. This application assumes that the default sigma for each category is used unless a sigma value is provided in the uploaded “sigma” column. Thus, this app requires either a category or a sigma to be specified. The app also assumes that all natural mortalities are less than 0.15 unless a natural mortality is provided in the uploaded “m” column."),
    p("Note that the app currently assumes that the provided buffer represents the size of the reduction (e.g., 6%) rather than the ratio of the ABC to the OFL (e.g., 94%)."),
    p("The code for the app is available here: ",
      tags$a(
        href = "https://github.com/cfree14/spex_checker/",
        "https://github.com/cfree14/spex_checker/",
        target = "_blank"
      )
    )  
  ),
  
  # Column definitions
  wellPanel(
    h3("Column definitions"),
    p("* indicates columns that are provided as inputs; other columns are derived by the app"),
    fluidRow(
      column(
        6,
        tags$ul(
          tags$li(tags$b("stock_id:*"), " Stock id"),
          tags$li(tags$b("assess_year:*"), " Year of assessment"),
          tags$li(tags$b("year:*"), " Year"),
          tags$li(tags$b("nyr_since_assessment:"), " Number of years since the assessment"),
          tags$li(tags$b("category:*"), " Category of assessment (1, 2, or 3)"),
          tags$li(tags$b("sigma:*"), " Base sigma"),
          tags$li(tags$b("m:*"), " Natural mortality rate"),
          tags$li(tags$b("slope:"), " Slope of the time-varying increase in sigma"),
          tags$li(tags$b("sigma_adj:"), " Time-varying sigma (uncapped)"),
          tags$li(tags$b("sigma_adj_cap:"), " Capped time-varying sigma (uncapped)")
        )
      ),
      column(
        6,
        tags$ul(
          tags$li(tags$b("pstar:*"), " Risk tolerance policy (P*)"),
          tags$li(tags$b("buffer:*"), " The reported buffer"),
          tags$li(tags$b("buffer_calc:"), " The buffer calculated by the app"),
          tags$li(tags$b("buff_diff:"), " The difference between the reported and calculated buffer (0=agreement)"),
          tags$li(tags$b("ofl:*"), " Overfishing limit (OFL)"),
          tags$li(tags$b("abc:*"), " Acceptable biological catch (ABC)"),
          tags$li(tags$b("abc_calc:"), " The ABC calculated by the app"),
          tags$li(tags$b("abc_diff:"), " The difference between the reported and calculated ABC (0=agreement)"),
          tags$li(tags$b("acl:*"), " Annual catch limit (ACL)")
        )
      )
    )
  ),
  
  # File upload
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
      )
      # column(
      #   width = 6,
      #   uiOutput("sheet_picker"),
      #   br(),
      #   downloadButton("download_csv", "Download table (CSV)")
      # )
    ),
    
    # Required columns
    uiOutput("messages"),  # explanatory text shown ABOVE the table
    
    # Checkbox
    checkboxInput(
      inputId = "only_diff",
      label   = "Reduce to rows where the buffers need checking:",
      value   = FALSE
    )
    
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
    "buffer", "ofl", "abc"
  )
  
  err_msg <- reactiveVal(NULL)
  
  processed <- reactive({
    req(data_orig())
    err_msg(NULL)
    
    # r <- 0.075  # fixed
    
    tryCatch({
      
      # Clean column names
      df <- data_orig() %>% 
        janitor::clean_names("snake")
      
      # Add sigma column if there isn't one
      sigma_yn <- "sigma" %in% colnames(df)
      if(sigma_yn==F){
        df$sigma <- NA
      }
      
      # Add M if there isn't one
      m_yn <- "m" %in% colnames(df)
      if(m_yn==F){
        df$m <- 0.15
      }
      
      # Check if columns are missing
      missing <- setdiff(required_cols, names(df))
      if (length(missing) > 0) {
        stop(sprintf(
          "Missing required columns after cleaning names: %s",
          paste(missing, collapse = ", ")
        ))
      }
      
      # Check if any columns aren't numbers
      numeric_cols <- c("year", "assess_year", "category", "pstar", "buffer", "ofl", "abc", "acl")
      for (cc in numeric_cols) {
        if (!is.numeric(df[[cc]])) suppressWarnings(df[[cc]] <- as.numeric(df[[cc]]))
      }
      
      # Build results table
      out <- df %>%
        # Calculaye years since last assessment
        mutate(nyr_since_assessed = year - assess_year) %>%
        # Fill missing sigmas based on category
        mutate(
          sigma = case_when(
            is.na(sigma) & category == 1 ~ 0.5,
            is.na(sigma) & category == 2 ~ 1.0,
            is.na(sigma) & category == 3 ~ 2.0,
            TRUE ~ sigma
          )
        ) %>%
        # Fill missing natural mortalities
        mutate(m=ifelse(is.na(m), 0.15, m)) %>% 
        # Add slope of time-varing sigma increase
        mutate(slope=ifelse(m<=0.15, 0.075, 0.52*m)) %>% 
        # Add time varying sigma
        mutate(sigma_adj=case_when(category==3 ~ 2.0,
                                   category %in% 1:2 ~ sigma * (1 + slope * (nyr_since_assessed-1)),
                                   T ~ NA)) %>% 
        # Cap sigma
        mutate(sigma_adj_cap=pmin(sigma_adj, 2)) %>% 
        # Calculate buffer
        mutate(buffer_calc = 1 - qlnorm(pstar, meanlog = 0, sdlog = sigma_adj_cap)) %>%
        # Calculate ABC
        mutate(abc_calc = ofl * (1 - buffer_calc)) %>%
        # Compute differences
        mutate(buffer_diff = abs(buffer_calc - buffer),
               abc_diff    = abs(abc_calc - abc)) %>%
        # Arrange
        select(
          stock_id, assess_year, year, nyr_since_assessed,
          category, sigma, m, slope, sigma_adj, sigma_adj_cap, pstar,
          buffer, buffer_calc, buffer_diff,
          ofl, abc, abc_calc, abc_diff, acl
        ) %>%
        # Round units for display (3 sig figs)
        mutate_at(vars(m, slope, 
                       sigma_adj, sigma_adj_cap, 
                       buffer, buffer_calc, buffer_diff), function(x) round(x, 3)) %>% 
        # Round units for display (1 sig figs)
        mutate_at(vars(abc_calc, abc_diff), function(x) round(x, 1)) 
      out
    },
    error = function(e) {
      err_msg(e$message)
      NULL
    })
  })
  
  # Filter to buff != 0
  filtered <- reactive({
    req(processed())
    if (isTRUE(input$only_diff)) {
      # Use a tiny tolerance in case of rounding
      dplyr::filter(processed(), abs(buffer_diff) > 1e-12)
    } else {
      processed()
    }
  })
  
  # Results table
  output$results_table <- renderDT({
    validate(
      need(!is.null(filtered()), if (!is.null(err_msg())) paste("Processing error:", err_msg()) else "Upload a file to begin.")
    )
    datatable(
      filtered(),
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
  
  # Download CSV button
  output$download_csv <- downloadHandler(
    filename = function() paste0("abc_buffer_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(processed())
      write_csv(processed(), file, na = "")
    }
  )
  
  # Required columns
  output$messages <- renderUI({
    tagList(
      tags$p(
        style = "color:#555;",
        "Required columns: ",
        code(paste(required_cols, collapse = ", ")), "."
      ),
      if (!is.null(err_msg()))
        tags$p(style = "color:#b00020; font-weight:600;", paste("Processing error:", err_msg()))
    )
  })
  
}

shinyApp(ui, server)


