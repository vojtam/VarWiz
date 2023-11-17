box::use(
  shiny[moduleServer, NS, tagList, renderPlot, plotOutput],
  plotly[plot_ly, layout, plotlyOutput, renderPlotly],
  data.table[...],
  stats[na.omit]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("sankey"))
  )
}

#' @export
server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    sankey_data <- dataset[1:20, .(gene_name, kegg_paths_name)]
    labels_all <- data.table(label = c(sankey_data$kegg_paths_name, sankey_data$gene_name))
    labels_all[, id := match(labels_all$label, unique(labels_all$label))]
    labels <- unique(labels_all)
    pathways <- unique(sankey_data[, .(kegg_paths_name)])[, source := rleid(kegg_paths_name) - 1]
    genes <- unique(sankey_data[, .(gene_name)])[, target := rleid(gene_name) - 1]
  
    
    with_source <- sankey_data[labels, on = c(kegg_paths_name = "label")]
    setnames(with_source, old = "id", new = "source")
    full <- with_source[labels, on = c(gene_name = "label")]
    setnames(full, old = "id", new = "target")
    full <- na.omit(full)
    create_sankey(labels, full)
    
    output$sankey <- renderPlotly({
      
    })
  })
}


create_sankey <- function(labels, sankey_data) {
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = labels$label,
      color = rep("blue", nrow(labels)),
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    link = list(
      source = sankey_data$source,
      target = sankey_data$target,
      value = rep(1, nrow(sankey_data))
    )
  )
  
  fig <- fig |> layout(
    title = "Sankey Diagram",
    font = list(
      size = 10
    )
  )
  
  
  fig
}

