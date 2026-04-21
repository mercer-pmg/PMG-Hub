# SSL Certificate Configuration----
# Configure SSL certificate verification using REQUESTS_CA_BUNDLE env var
# Set REQUESTS_CA_BUNDLE to the path of your CA certificate bundle (e.g. cacert.pem)
cert_path <- Sys.getenv("REQUESTS_CA_BUNDLE")
if (nzchar(cert_path) && file.exists(cert_path)) {
  # Set environment variables for curl and httr packages
  Sys.setenv(CURL_CA_BUNDLE = cert_path)
  Sys.setenv(HTTR_CA_BUNDLE = cert_path)

  # Configure httr options if the package is loaded
  if (requireNamespace("httr", quietly = TRUE)) {
    httr::set_config(httr::config(cainfo = cert_path))
  }

  # Configure curl options if the package is loaded
  if (requireNamespace("curl", quietly = TRUE)) {
    options(curl_ca_bundle = cert_path)
  }
} else if (nzchar(cert_path)) {
  warning(
    "SSL certificate file not found at REQUESTS_CA_BUNDLE: ",
    cert_path,
    ". SSL verification may not work correctly."
  )
}

# Data Set-Up----
strategy_aum <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  object = "strategies-aum.csv",
  bucket = "aspen-investing-menu"
) |>
  readBin("character") |>
  (\(x) readr::read_csv(I(x), show_col_types = FALSE))()

orion_platform <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  object = "orion-platform.csv",
  bucket = "aspen-investing-menu"
) |>
  readBin("character") |>
  (\(x) readr::read_csv(I(x), show_col_types = FALSE))() |>
  kdot::clean_orion_platform(
    drop_columns = c("asset_category", "market_cap", "is_SMA")
  ) |>
  dplyr::left_join(
    y = strategy_aum,
    by = dplyr::join_by("strategy" == "Strategy Name")
  )


category_list <- unique(orion_platform$category)
type_list <- unique(orion_platform$type)
model_list <- orion_platform |>
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
    titleWidth = 210
  )

sidebar <-
  shinydashboard::dashboardSidebar(
    width = 210,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text = "Aspen Investing Menu",
        tabName = "aim"
      ),
      shinydashboard::menuItem(
        text = "Suite Builder",
        tabName = "suite_builder"
      ),
      shinydashboard::menuItem(
        text = "Blended Strategy Bundler",
        tabName = "blended_bundler"
      ),
      shinydashboard::menuItem(
        text = "Portfolio Builder",
        tabName = "portfolio_builder"
      ),
      shinydashboard::menuItem(
        text = "Orion Products Audit",
        tabName = "products_audit"
      ),
      shinydashboard::menuItem(
        text = "Orion SMA Settings",
        tabName = "orion_sma"
      )
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
      ),

      ## Orion SMA Settings-----------------------------------------------------
      shinydashboard::tabItem(
        tabName = "orion_sma",
        orionSmaUI("orion_sma")
      )
    )
  )

ui <- shinydashboard::dashboardPage(header, sidebar, body)

# Server----
server <- function(input, output, platform = orion_platform) {
  options(shiny.maxRequestSize = 500 * 1024^2)

  ## Aspen Investing Menu----
  aimServer("aim", platform = orion_platform)

  ## Suite Builder----
  suite_builderServer("suite_builder", platform = orion_platform)

  ## Blended Strategy Bundler----
  bundle <- bs_bundlerServer("blended_bundler")

  ## Orion Products Audit----
  productsAuditServer("products_audit")

  ## Portfolio Builder----
  portfolioBuilderServer("portfolio_builder")

  ## Orion SMA Settings----
  orionSmaServer("orion_sma")
}

shinyApp(ui = ui, server = server)
