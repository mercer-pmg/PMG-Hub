
strategy_aum <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  
  object = "strategies-aum.csv", 
  bucket = "aspen-investing-menu") |>
  readBin("character") |>
  readr::read_csv(show_col_types = FALSE)

orion_platform <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  
  object = "orion-platform.csv", 
  bucket = "aspen-investing-menu") |>
  readBin("character") |>
  readr::read_csv(show_col_types = FALSE)

orion_platform <- orion_platform |>

  dplyr::mutate(
    
    category = NA,
    
    category = dplyr::if_else(
      condition = type == "Blended Strategy",
      true      = "Blended",
      false     = category),
    
    category = dplyr::if_else(
      condition = type %in% c("Market Series", "Multifactor Series", "Income Series"),
      true      = "Risk-Based",
      false     = category),
    
    category = dplyr::if_else(
      condition = type %in% c("Equity Strategies", "Fixed Income Strategies", "Cash Strategies", "Alternative Strategies", "Special Situation Strategies"),
      true      = "Asset Class",
      false     = category)
  ) |>
  
  dplyr::mutate(
    model_group = "Other",
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Market"),
      true      = "Market Series",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Multifactor"),
      true      = "Multifactor Series",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Income"),
      true      = "Income Series",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Cash Mgmt"),
      true      = "Cash Mgmt",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "Quantitative Portfolio"),
      true      = "Quantitative Portfolios",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Fixed Income"),
      true      = "Fixed Income",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "Ladder \\(ETF\\)$"),
      true      = "Fixed Income ETF Ladder",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "BlackRock|Nuveen|PIMCO"),
      true      = "Third-Party Fixed Income SMA",
      false     = model_group
    )
    
  )

orion_platform <- dplyr::left_join(
  x = orion_platform,
  y = strategy_aum,
  by = dplyr::join_by("strategy" == "Strategy Name")
)


category_list <- unique(orion_platform$category)
type_list     <- unique(orion_platform$type)
model_list    <- orion_platform |>
  dplyr::select(model_group, model_agg) |>
  dplyr::arrange(model_agg) |>
  dplyr::distinct() |>
  dplyr::group_by(model_group) |>
  dplyr::summarise(model_ls = list(model_agg)) |>
  tibble::deframe()


# UI----
header <- 
  shinydashboard::dashboardHeader(
    title = "PMG Apps",
    titleWidth = 210)

sidebar <- 
  shinydashboard::dashboardSidebar(
    width = 210,
    shinydashboard::sidebarMenu(
      
      shinydashboard::menuItem(
        text = "Aspen Investing Menu",
        tabName = "aim"
      ),
      
      shinydashboard::menuItem(
        text    = "Suite Builder", 
        tabName = "suite_builder"),
      
      shinydashboard::menuItem(
        text    = "Blended Strategy Bundler", 
        tabName = "blended_bundler"),
      
      shinydashboard::menuItem(
        text    = "Portfolio Builder", 
        tabName = "portfolio_builder"),
      
      shinydashboard::menuItem(
        text    = "Orion Products Audit", 
        tabName = "products_audit")
    )
  )

body <-
  shinydashboard::dashboardBody(
    
    shiny::includeCSS("www/custom.css"),
    
    shinydashboard::tabItems(
      
      ## Aspen Investing Menu---------------------------------------------------
      shinydashboard::tabItem(
        tabName = "aim",
        aimUI("aim", platform = orion_platform)
      
        ),
      
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
        
      ),
      
      ## Orion Products Audit---------------------------------------------------
      shinydashboard::tabItem(
        tabName = "products_audit",
        productsAuditUI("products_audit")
      )
    )
  )

ui     <- shinydashboard::dashboardPage(header, sidebar, body)

# Server----
server <- function(input, output, platform = orion_platform) {
  
  options(shiny.maxRequestSize=50*1024^2)
  
  ## Aspen Investing Menu----
  aimServer("aim", platform = orion_platform)
  
  ## Suite Builder----
  suite_builderServer("suite_builder", platform = orion_platform)
  
  ## Blended Strategy Bundler----
  bundle <- bs_bundlerServer("blended_bundler")
  
  ## Orion Products Audit----
  productsAuditServer("products_audit")
  
}

shinyApp(ui = ui, server = server)
