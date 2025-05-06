
orion_platform <- aws.s3::get_object(
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
        suite_builderUI("suite_builder", platform = orion_platform)
        
      ),
      
      ## Blended Strategy Bundler-----------------------------------------------
      shinydashboard::tabItem(
        tabName = "blended_bundler",
        bs_bundlerUI("blended_bundler"),
        
      ),
      
      ## Portfolio Builder------------------------------------------------------
      shinydashboard::tabItem(
        tabName = "portfolio_builder",
        portfolioBuilderUI("portfolio_builder")
      )
    )
  )

ui     <- shinydashboard::dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  # Suite Builder
  suite_builderServer("suite_builder", platform = orion_platform)
  
  # Blended Strategy Bundler
  bundle <- bs_bundlerServer("blended_bundler")
  
}

shinyApp(ui = ui, server = server)
