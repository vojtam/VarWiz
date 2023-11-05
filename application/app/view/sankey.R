box::use(
  shiny[moduleServer, NS, tagList, renderPlot, plotOutput],
  plotly[plot_ly, layout, plotlyOutput, renderPlotly],
  data.table[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("sankey"))
  )

}

#' @export
server <- function(id, my_dataset) {
  moduleServer(id, function(input, output, session) {
    
    
    output$sankey <- renderPlotly({
      print(my_dataset)
      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = unique(my_dataset$kegg_paths_name),
          pad = 15,
          thickness = 20,
          line = list(
            color = "black",
            width = 0.5
          )
        )
      )
    })
    
    
  })
}
