box::use(
  shiny[moduleServer, NS, tagList, h3],
  DT[renderDataTable, dataTableOutput]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("TABLE"),
    dataTableOutput(ns("table"))
  )

}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDataTable(
      data
    )
  })
}
