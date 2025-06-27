
aimUI <- function(id, platform, categories, types, models){
  
  ns <- shiny::NS(id)
  
  category_list <- unique(platform$category)
  type_list     <- unique(platform$type)
  model_list    <- platform |>
    dplyr::select(model_group, model_agg) |>
    dplyr::arrange(model_agg) |>
    dplyr::distinct() |>
    dplyr::group_by(model_group) |>
    dplyr::summarise(model_ls = list(model_agg)) |>
    tibble::deframe()
  
  shiny::tagList(
    h1("Aspen Investing Menu"),
    
    "Under development.",
    
    br(),
    br(),
    
    sidebarLayout(
      sidebarPanel(width = 3,
                   
                   shiny::checkboxGroupInput(
                     inputId = ns("strategy_category"),
                     label   = "Category",
                     choices = category_list),
                   
                   
                   checkboxGroupInput(
                     inputId = ns("strategy_types"),
                     label   = "Type",
                     choices = type_list),
                   
                   shiny::sliderInput(
                     input     = ns("equityAlloc"),
                     label     = "Equity Allocation Range",
                     min       = 0,
                     max       = 100,
                     value     = c(0,100),
                     step      = 10,
                     dragRange = TRUE),
                   
                   checkboxGroupInput(
                     inputId      = ns("tm_status"),
                     label        = "Tax-Managed",
                     choiceNames  = c("Yes", "No"),
                     choiceValues = c(TRUE, FALSE),
                     inline       = TRUE),
                   
                   shiny::radioButtons(
                     inputId  = ns("model_search"),
                     label    = "Search by Model",
                     choices  = c("Any Model", "All Models"),
                     selected = "Any Model",
                     inline   = TRUE),
                   
                   shiny::selectInput(
                     inputId   = ns("models"),
                     label     = NULL,
                     choices   = model_list,
                     multiple  = TRUE,
                     selectize = TRUE)
                   
                   
      ),
      
      mainPanel(
        shiny::textOutput(ns("message")),
        DT::DTOutput(ns("strategies"))
        
      )
      
    )
  )
  
  
}

aimServer <- function(id, platform) {
  
  ns <- shiny::NS(id)
  
  category_list <- unique(platform$category)
  type_list     <- unique(platform$type)
  model_list    <- platform |>
    dplyr::select(model_group, model_agg) |>
    dplyr::arrange(model_agg) |>
    dplyr::distinct() |>
    dplyr::group_by(model_group) |>
    dplyr::summarise(model_ls = list(model_agg)) |>
    tibble::deframe()
  
  moduleServer(id, function(input, output, session, dat = platform) {
    
    # UI updates on category selection
    shiny::observeEvent(input$strategy_category, {
      
      if(is.null(input$strategy_category)) {
        
        shiny::updateCheckboxGroupInput(
          inputId = "strategy_types",
          choices = type_list
        )
        
        shiny::updateSelectInput(
          inputId = "models",
          choices = model_list
        )
      
      } else {
        
        print(input$strategy_category)
        
        print(platform)
        
        shiny::updateCheckboxGroupInput(
          inputId = "strategy_types",
          choices = platform |>
            dplyr::filter(category %in% input$strategy_category) |>
            dplyr::pull(type) |>
            unique()
        )

        shiny::updateSelectInput(
          inputId = "models",
          choices = platform |>
            dplyr::filter(category %in% input$strategy_category) |>
            dplyr::select(model_group, model_agg) |>
            dplyr::arrange(model_agg) |>
            dplyr::distinct() |>
            dplyr::group_by(model_group) |>
            dplyr::summarise(model_ls = list(model_agg)) |>
            tibble::deframe()
        )
      }
    },
    ignoreNULL = FALSE)
    
    # UI updates on type selection
    shiny::observeEvent(
      eventExpr = input$strategy_types, 
      handlerExpr = {
        
        if(is.null(input$strategy_types)) {
          shiny::updateCheckboxGroupInput(
            inputId = "strategy_types",
            choices = type_list
          )
          
          shiny::updateSelectInput(
            inputId = "models",
            choices = model_list
          )
        }
        
        shiny::updateSelectInput(
          inputId = "models",
          choices = platform |>
            dplyr::filter(type %in% input$strategy_types) |>
            dplyr::select(model_group, model_agg) |> 
            dplyr::arrange(model_agg) |> 
            dplyr::distinct() |> 
            dplyr::group_by(model_group) |>
            dplyr::summarise(model_ls = list(model_agg)) |>
            tibble::deframe()
        )
      }
    )
    
    # Filtering functions
    cat <- reactive({
      
      if(length(input$strategy_category) == 0) {
        unique(platform$category)
      } else {
        input$strategy_category
      }
      
    })
    
    
    
    types <- reactive({
      
      if(length(input$strategy_types) == 0) {
        unique(platform$type)} else {
          input$strategy_types
        }
      
    })
    
    equity_allo <- reactive({
      
      eq_range <- seq(
        from = input$equityAlloc[1], 
        to   = input$equityAlloc[2], 
        by   = 10)
      
      c(NA, eq_range)
      
    })
    
    tax_mgmt <- reactive({
      
      if(length(input$tm_status) == 0) {
        unique(platform$tax_managed)
      } else {
        input$tm_status
      }
      
    })
    
    models <- reactive({
      
      if(length(input$models) == 0) {
        unique(platform$model_agg)
      } else {
        input$models
      }
      
    })
    
    # Filtering platform based on filtering functions
    dat <- reactive({
      
      df <- platform |>
        dplyr::filter(category %in% cat()) |>
        dplyr::filter(type %in% types()) |>
        dplyr::filter(portfolio %in% equity_allo()) |>
        dplyr::filter(tax_managed %in% tax_mgmt()) |>
        dplyr::filter(model_agg %in% models())
      
      if(input$model_search == "All Models") {
        
        models_n <- length(models())
        
        
        xx <- platform |>
          dplyr::select(strategy, model_agg) |>
          dplyr::group_by(strategy) |>
          dplyr::summarise(n = dplyr::n()) |>
          dplyr::filter(n == models_n) |>
          dplyr::pull(strategy)
        
        df <- df |> 
          dplyr::filter(strategy %in% xx)
        
        
      }
      
      df <- df |>
        dplyr::select(strategy) |>
        dplyr::distinct()
      
      print(df)
      
      df <- df |>
        dplyr::left_join(
          platform |> dplyr::select(strategy, N, AUM) |> dplyr::distinct(),
          by = "strategy") |>
        
        dplyr::rename(
          Registrations = N,
          Strategy      = strategy
        )
      
    })
    
    output$message <- shiny::renderText(paste(nrow(dat()), "strategies returned"))
    
    output$strategies <- DT::renderDataTable({
      DT::datatable(
        dat(),
        rownames = FALSE,
        options = list(paging = FALSE),
        escape  = FALSE) |>
        DT::formatCurrency("AUM") |>
        DT::formatCurrency("Registrations", currency = "", mark = ",", digits = 0)
      
    }) 
  }
  )
}