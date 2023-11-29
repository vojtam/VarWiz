box::use(
  shiny[moduleServer, NS, h1, fluidRow, column],
  DT[renderDT, dataTableOutput],
  htmlwidgets[JS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      dataTableOutput(ns("browser_table"))
    )
  )

}

#' @export
server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    output$browser_table <- renderDT({
      

      DT::datatable(
        dataset,
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#001f3f', 'color': 'white'});",
            "}")
        )
      )
    })
  })
}
