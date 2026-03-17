portfolioBuilderUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    h1("Portfolio Builder"),
    
    "Define asset allocation of a portfolio. Review expected outcomes.",
    
    br(),
    br(),
    
    shiny::fluidPage(
      
      fluidRow(
        column(width = 3, h4("Asset Classes")),
        column(width = 1, h4("Portfolio A"))
      ),
      
      fluidRow(
        column(
          width = 3,
          shiny::selectInput(
            inputId   = ns("asset_classes1"),
            label     = NULL,
            choices   = c("Choose Asset Class" = "", kdot::get_asset_classes()),
            selectize = TRUE,
            width     = "100%")
        ),
        
        column(
          width = 1,
          numericInput(
            inputId   = ns("weight1"), 
            label     = NULL, 
            value     = 0,
            min       = 0,
            max       = 100,
            step      = 1,
            width     = "100%")
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          shiny::selectInput(
            inputId   = ns("asset_classes2"),
            label     = NULL,
            choices   = c("Choose Asset Class" = "", kdot::get_asset_classes()),
            selectize = TRUE,
            width     = "100%")
        ),
        
        column(
          width = 1,
          numericInput(
            inputId   = ns("weight2"), 
            label     = NULL, 
            value     = 0,
            min       = 0,
            max       = 100,
            step      = 1,
            width     = "100%")
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          shiny::selectInput(
            inputId   = ns("asset_classes3"),
            label     = NULL,
            choices   = c("Choose Asset Class" = "", kdot::get_asset_classes()),
            selectize = TRUE,
            width     = "100%")
        ),
        
        column(
          width = 1,
          numericInput(
            inputId   = ns("weight3"), 
            label     = NULL, 
            value     = 0,
            min       = 0,
            max       = 100,
            step      = 1,
            width     = "100%")
        )
      )
    )
  )
}


#' Portfolio Builder server - placeholder
#'
#' Minimal server for Portfolio Builder. UI provides asset class and weight inputs;
#' expected outcomes / calculations are not yet implemented.
#'
#' @param id Module namespace id
portfolioBuilderServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Placeholder: no outputs wired yet. Asset classes and weights are available
    # via input$asset_classes1-3 and input$weight1-3 when full feature is scoped.
  })
}