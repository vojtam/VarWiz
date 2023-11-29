box::use(
  shiny[moduleServer, NS, req, validate, need, tagList, renderPlot, plotOutput, observeEvent, reactiveVal],
  plotly[plot_ly, layout, plotlyOutput, renderPlotly],
  data.table[...],
  bs4Dash[box],
  stats[na.omit]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      height = 1000,
      title = "Sankey Diagram",
      status = "navy",
      solidHeader = TRUE,
      id = "sankey_box",
      plotlyOutput(ns("sankey"))
    )
  )
}

#' @export
server <- function(id, dataset, selected_pathways_react) {
  moduleServer(id, function(input, output, session) {
    
    sankey_plot <- reactiveVal()
    
    observeEvent(selected_pathways_react(), {
      req(selected_pathways_react())
      data <- sankey_prepare_data(dataset, selected_pathways_react())
      plot <- create_sankey(data[[1]], data[[2]])
      sankey_plot(plot)
    })
    
    
    output$sankey <- renderPlotly({
      validate(
        need(!is.null(selected_pathways_react()), "From the table select the pathways that you want to visualize")
      )
      req(sankey_plot())
      sankey_plot()
    })
  })
}

sankey_prepare_data <- function(dataset, selected_pathways) {
  
  sankey_data <- dataset[kegg_paths_name %in% selected_pathways$pathway, .(gene_name, kegg_paths_name)]
  labels_all <- data.table(
    label = c(sankey_data$kegg_paths_name, sankey_data$gene_name)
  )
  labels_all[, id := match(labels_all$label, unique(labels_all$label))]
  labels <- unique(labels_all)
  pathways <- unique(sankey_data[, .(kegg_paths_name)])[, source := rleid(kegg_paths_name) - 1]
  genes <- unique(sankey_data[, .(gene_name)])[, target := rleid(gene_name) - 1]
  
  
  with_source <- sankey_data[labels, on = c(kegg_paths_name = "label")]
  setnames(with_source, old = "id", new = "source")
  full <- with_source[labels, on = c(gene_name = "label")]
  setnames(full, old = "id", new = "target")
  full <- unique(na.omit(full))
  return(list(labels, full))
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
      source = sankey_data$source -1,
      target = sankey_data$target - 1,
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

