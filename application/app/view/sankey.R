box::use(
  shiny[moduleServer, NS, tagList, renderPlot, plotOutput],
  plotly[plot_ly, layout]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("sankey"))
  )

}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$sankey <- renderPlot({
      
    })
  })
}
