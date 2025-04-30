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

      
## Suite Builder ---------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "suite_builder",
        h1("Suite Builder"),
        
        "Define equity and fixed income allocations. Generate suite of sleeve strategies.",
        
        br(),
        br(),
        
        shiny::textInput(
          inputId = "suite",
          label = "Suite Name"
        ),
        
        shiny::checkboxGroupInput(
          inputId  = "strategies", 
          label    = "Strategies to Build",
          inline   = TRUE,
          choices  = seq(from = 100, to = 10, by = -10),
          selected = seq(from = 100, to = 10, by = -10)
        ),
        
        fluidRow(
          column(
            6,
            h3("Equity Sleeves"),
            
            column(
              9,
              shiny::selectInput(
                inputId = "equitySleeve1",
                label     = NULL, 
                choices   = c("Choose Sleeve" = "", unique(platform$model_agg)),
                selectize = TRUE,
                width     = "100%"
              ),
              
              shiny::selectInput(
                inputId = "equitySleeve2",
                label     = NULL, 
                choices   = c("Choose Sleeve" = "", "A", "B", "C"),
                selectize = TRUE,
                width     = "100%"
              )
            ),
            
            column(
              3,
              shiny::numericInput(
                inputId   = "equityWeight1", 
                label     = NULL, 
                value     = 0,
                min       = 0,
                max       = 100,
                step      = 1,
                width     = "100%"),
              
              shiny::numericInput(
                inputId   = "equityWeight2", 
                label     = NULL, 
                value     = 0,
                min       = 0,
                max       = 100,
                step      = 1,
                width     = "100%")
            )
            
          ),
          
          column(
            6,
            h3("Fixed Income Sleeves"),
            
            column(
              9,
              shiny::selectInput(
                inputId = "fixedSleeve1",
                label     = NULL, 
                choices   = c("Choose Sleeve" = "", "A", "B", "C"),
                selectize = TRUE,
                width     = "100%"
              ),
              
              shiny::selectInput(
                inputId = "fixedSleeve2",
                label     = NULL, 
                choices   = c("Choose Sleeve" = "", "A", "B", "C"),
                selectize = TRUE,
                width     = "100%"
              )
            ),
            
            column(
              3,
              shiny::numericInput(
                inputId   = "fixedWeight1", 
                label     = NULL, 
                value     = 0,
                min       = 0,
                max       = 100,
                step      = 1,
                width     = "100%"),
              
              shiny::numericInput(
                inputId   = "fixedWeight2", 
                label     = NULL, 
                value     = 0,
                min       = 0,
                max       = 100,
                step      = 1,
                width     = "100%"
              )
            )
          )
        )
      ),

## Blended Strategy Bundler-----------------------------------------------------
      shinydashboard::tabItem(
        tabName = "blended_bundler",
        h2("Blended Strategy Bundler")
      ),

## Portfolio Builder------------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "portfolio_builder",
        h2("Portfolio Builder")
      )
    )
  )

ui     <- shinydashboard::dashboardPage(header, sidebar, body)
server <- function(input, output) {}

shinyApp(ui = ui, server = server)
