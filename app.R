
source("R/bs_bundler.R")

platform <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  
  object = "orion-platform.csv", 
  bucket = "aspen-investing-menu") |>
  readBin("character") |>
  readr::read_csv(show_col_types = FALSE)



header <- 
  shinydashboard::dashboardHeader(
    title = "PMG Hub",
    titleWidth = 210)

sidebar <- 
  shinydashboard::dashboardSidebar(
    width = 210,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Suite Builder", tabName = "suite_builder"),
      shinydashboard::menuItem("Blended Strategy Bundler", tabName = "blended_bundler"),
      shinydashboard::menuItem("Portfolio Builder", tabName = "portfolio_builder")
    )
  )

body <-
  shinydashboard::dashboardBody(
    
    shiny::includeCSS("www/custom.css"),
    
    shinydashboard::tabItems(
      
      
      ## Suite Builder ---------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "suite_builder",
        h1("Suite Builder"),
        
        "Define equity and fixed income allocations. Generate a suite of sleeve strategies.",
        
        br(),
        br(),
        
        fluidRow(
          column(4,shiny::textInput(inputId = "suite", label = "Suite Name")),
          column(6,
                 shiny::checkboxGroupInput(
                   inputId  = "strategies", 
                   label    = "Strategies to Build",
                   inline   = TRUE,
                   choices  = seq(from = 100, to = 10, by = -10),
                   selected = seq(from = 100, to = 10, by = -10)))
          
        ),
        
        
        
        fluidRow(
          column(6, h3("Equity Sleeves"),
                 
                 column(9, paste0("equity", 1:7) |> purrr::map(sleeve_input, df = platform)),
                 column(3, paste0("equity_weight", 1:7) |> purrr::map(weight_input))
                 
          ),
          
          column(6, h3("Fixed Income Sleeves"),
                 
                 column(
                   9,
                   paste0("fixed", 1:7) |> purrr::map(sleeve_input, df = platform)
                 ),
                 
                 column(
                   3,
                   paste0("fixed_weight", 1:7) |> purrr::map(weight_input)
                 )
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          column(2, shiny::uiOutput("download"))
        )
      ),
      
      ## Blended Strategy Bundler-----------------------------------------------
      shinydashboard::tabItem(
        tabName = "blended_bundler",
        bs_bundlerUI("blended_bundler"),
        
      ),
      
      ## Portfolio Builder------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "portfolio_builder",
        tibble::tibble("Asset" = "U.S. Large Cap", Weight = 1) |> kdot::expected_return()
      )
    )
  )

ui     <- shinydashboard::dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  # Suite Builder
  output$downloadSuite <- downloadHandler(

    filename = function() {
      paste0(input$suite, ".xlsx")
    },

    content = function(file) {
      openxlsx::write.xlsx(x = input$strategies |>
                             purrr::map_dfr(
                               .f         = build_suite,
                               x          = equity_allocation(input),
                               y          = fixed_allocation(input),
                               suite_name = input$suite,
                               ids        = platform),
                           file)
    }
  )

  output$download <- renderUI(downloadButton("downloadSuite", "Download", width = "100%"))
  
  
  # Blended Strategy Bundler
  bundle <- bs_bundlerServer("blended_bundler")
  
}

shinyApp(ui = ui, server = server)
