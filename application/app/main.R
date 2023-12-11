box::use(
  shiny[bootstrapPage, div, icon, moduleServer, fluidRow, NS, renderUI, tags, uiOutput],
  bs4Dash[box, dashboardPage, tabItems, tabItem, sidebarMenu, menuItem, dashboardHeader, column, dashboardBrand, dashboardSidebar, dashboardBody],
  thematic[thematic_shiny],
  shinyjs[useShinyjs, runjs]
)

box::use(
  app/logic/data_preprocessor[preprocess_data]
)

box::use(
  app/view/table,
  app/view/sankey,
  app/view/lollipop,
  app/view/browser
)

thematic_shiny()

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    header = dashboardHeader(
      title = "VarWiz",
      skin = "dark",
      status = "white"
    ),
    dark = NULL,
    help = NULL,
    sidebar = dashboardSidebar(
      collapsed = TRUE,
      status = "navy",
      skin = "dark",
      sidebarMenu(
        id = "side_menu",
        div(class = "menu-tab", menuItem("Visualization", tabName = "visualization_tab", icon = icon("chart-line"))),
        div(class = "menu-tab", menuItem("Browse data", tabName = "browse_tab", icon = icon("table"))),
        div(class = "menu-tab", menuItem("About", tabName = "about_tab", icon = icon("book-open"))),
        div(class = "menu-tab", menuItem("Start Tutorial", tabName = "tutorial", icon = icon("play")))
      )
    ),
    body = dashboardBody(
      tags$head(
        tags$script(
          "$(function() {
              $('[data-card-widget=\"maximize\"]').on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                  if (isMaximized) {
                    $('#app-sankey-sankey').css('height', '1000px');
                  } else {
                    $('#app-sankey-sankey').css('height', '400px');
                  }
                }, 300);
                $('#app-sankey-sankey').trigger('resize');
              });
            });
            "
        )
      ),
      
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "visualization_tab",
          fluidRow(
            column(
              width = 9,
              sankey$ui(ns("sankey")),
              lollipop$ui(ns("lollipop"))
            ),
            column(
              width = 3,
              table$ui(ns("table")),
            ),
          )
        ),
        
        tabItem(
          tabName = "browse_tab",
          browser$ui(ns("browser"))
          
        ),
        
        tabItem(
          tabName = "about_tab"
        )
      )
      
      
      
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- preprocess_data()
    browser$server("browser", data)
    selected_pathways <- table$server("table", data)
    selected_gene_rval <- sankey$server("sankey", data, selected_pathways)
    lollipop$server("lollipop", data, selected_gene_rval)
  })
}
