box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
  bs4Dash[box, dashboardPage, dashboardHeader, dashboardBrand, dashboardSidebar, dashboardBody]
)

box::use(
  app/logic/data_preprocessor[preprocess_data]
)

box::use(
  app/view/table,
  app/view/sankey
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    header = dashboardHeader(
      title = "VarWiz"
    ),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      table$ui(ns("table")),
      box(
        title = "Sankey Diagram",
        id = "sankey_box",
        sankey$ui(ns("sankey"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- preprocess_data()
    table$server("table", data)
    sankey$server("sankey")
  })
}
