box::use(
  shiny[...],
  bs4Dash[box, dashboardPage, tabItems, tabItem, sidebarMenu, menuItem, dashboardHeader, column, dashboardBrand, dashboardSidebar, dashboardBody],
  thematic[thematic_shiny],
  shinyjs[useShinyjs, runjs],
  gargoyle[init],
  utils[globalVariables]
)
globalVariables(c("mutation.table.df", "hgnc2pfam.df"))

library(g3viz)
box::use(
  app/logic/data_preprocessor[preprocess_data],
  app/logic/data_preprocessor[preprocess_maf_data]
)

box::use(
  app/view/table,
  app/view/sankey,
  app/view/lollipop,
  app/view/browser,
  app/view/plot_options
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
              plot_options$ui(ns("plot_options")),
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


whole_dataset <- preprocess_maf_data()

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    init("reset_sankey")

    ns <- NS(id)
    data <- reactiveVal(whole_dataset)
    options <- plot_options$server("plot_options", unique(data()$Variant_Type), unique(data()$Consequence))
    observeEvent(options(), {
      req((length(options()[[1]]) > 0 || length(options()[[2]] > 0)))

      selected_var_types <- options()[[1]]
      selected_consequences <- options()[[2]]
      
      tab <- whole_dataset[Variant_Type %in% selected_var_types]
      data(tab[Consequence %in% selected_consequences])
      
    })

    # data <- preprocess_data()
    browser$server("browser", data())
    
    selected_features <- table$server("table", data())
    selected_gene_rval <- sankey$server("sankey", data, selected_features()[[1]], selected_features()[[2]], selected_features()[[3]])
    lollipop$server("lollipop", data, selected_gene_rval)
  })
}
