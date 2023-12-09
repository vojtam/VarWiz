box::use(
  shiny[moduleServer, isolate, NS, req, validate, need, tagList, renderPlot, plotOutput, observeEvent, reactiveVal],
  plotly[plot_ly, layout, plotlyOutput, renderPlotly],
  data.table[...],
  bs4Dash[box],
  stats[na.omit],
  shinyjs[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      height = "50vh",
      title = "Sankey Diagram",
      status = "navy",
      maximizable = TRUE,
      solidHeader = TRUE,
      id = ns("sankey_box"),
      plotlyOutput(ns("sankey_plt"), height = "50vh")
    )
  )
}

#' @export
server <- function(id, dataset, selected_pathways_react) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    # reactive value holding the sankey plot
    sankey_plot <- reactiveVal()
    
    # data is passed into module -> create sankey
    observeEvent(selected_pathways_react(), {
      req(selected_pathways_react())
      data <- sankey_prepare_data(dataset, selected_pathways_react())
      plot <- create_sankey(data[[1]], data[[2]])
      sankey_plot(plot)
    })
    
    
    output$sankey_plt <- renderPlotly({
      validate(
        need(!is.null(selected_pathways_react()), "select the pathways that you want to visualize from the table on the right")
      )
      # output the contents of the sankey_plot reactive value
      sankey_plot()
    })
    observeEvent(input$sankey_box$maximized, {
      print("MAXIM")
      plot_height <- if (input$sankey_box$maximized) {
        "100%"
      } else {
        "50vh"
      }
      
      js_call <- sprintf(
        "
      console.log('TRIGGER');
      setTimeout(() => {
        $('#%s').css('height', '%s');
      }, 500)
      $('#%s').trigger('resize');
      $('#%s').trigger('resize');
      ",
      "app-sankey-sankey_plt",
      plot_height,
      "app-sankey-sankey_plt",
      "app-sankey-sankey_plt"
      )
      shinyjs::runjs(js_call)
      shinyjs::runjs("console.log('Hello JS')")
    }, ignoreInit = TRUE)
  
     
  })
}

sankey_prepare_data <- function(dataset, selected_pathways) {
  # filter the selected_pathways -> | gene_name | kegg_paths_name |
  sankey_data <- dataset[kegg_paths_name %in% selected_pathways$pathway, .(gene_name, kegg_paths_name, var_name)]
  labels_all <- data.table(
    label = c(sankey_data$kegg_paths_name, sankey_data$gene_name, sankey_data$var_name)
  )
  # get all labels their id
  labels_all[, id := match(labels_all$label, unique(labels_all$label))]
  labels <- unique(labels_all)
  
  with_source <- sankey_data[labels, on = c(kegg_paths_name = "label")]
  with_source <- with_source[labels, on = c(gene_name = "label")]
  with_source <- with_source[labels, on = c(var_name = "label")]
  setnames(with_source, old = c("id", "i.id", "i.id.1"), new = c("first", "second", "third"))
  full <- unique(na.omit(with_source))
  return(list(labels, full))
}

create_sankey <- function(labels, sankey_data) {
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = labels$label,
      color = "black",
      pad = 10,
      thickness = 20,
      line = list(
        color = "black",
        width = 2
      )
    ),
    link = list(
      source = c(sankey_data$first, sankey_data$second) - 1,
      target = c(sankey_data$second, sankey_data$third) - 1,
      value = rep(1, nrow(sankey_data) * 2)
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



#' Add a box maximization observer to automatically resize a plot in that box.
#'
#' @param input The input of a shiny app session.
#' @param box_id The shiny ID of the box to observe.
#' @param plot_name The shiny ID of the plot to resize.
#' @param non_max_height The height that the graph should be when the box is
#'   not maximized. Defaults to "400px".
add_plot_maximize_observer <- function(input,
                                       box_id,
                                       plot_name,
                                       non_max_height = "400px") {
  observeEvent(input$box_id$maximized, {
    print("MAXIM")
    plot_height <- if (input$box_id$maximized) {
      "100%"
    } else {
      non_max_height
    }
    
    js_call <- sprintf(
      "
      console.log('TRIGGER');
      setTimeout(() => {
        $('#%s').css('height', '%s');
      }, 300)
      $('#%s').trigger('resize');
      ",
      plot_name,
      plot_height,
      plot_name
    )
    shinyjs::runjs(js_call)
    shinyjs::runjs("console.log('Hello JS')")
  }, ignoreInit = TRUE)
}

