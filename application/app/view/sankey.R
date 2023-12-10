box::use(
  shiny[moduleServer, verbatimTextOutput, renderPrint, isolate, NS, req, validate, need, tagList, renderPlot, plotOutput, observeEvent, reactiveVal, reactiveValues],
  plotly[plot_ly, event_data, plotlyOutput, renderPlotly, plotlyProxy, plotlyProxyInvoke],
  data.table[...],
  bs4Dash[box],
  stats[na.omit],
  shinyjs[...]
)

box::use(
  app/logic/create_sankey[sankey_prepare_data, create_sankey]
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
    labels <- reactiveVal()
    path_gene_tab <- reactiveVal()
    gene_variant_tab <- reactiveVal()
    

# Render ------------------------------------------------------------------

    output$sankey_plt <- renderPlotly({
      validate(
        need(!is.null(selected_pathways_react()), "select the pathways that you want to visualize from the table on the right")
      )
      # output the contents of the sankey_plot reactive value
      sankey_plot()
    })
    
    
    

# Events ------------------------------------------------------------------

    # data is passed into module -> create sankey
    observeEvent(selected_pathways_react(), {
      req(selected_pathways_react())
      
      data <- sankey_prepare_data(dataset, selected_pathways_react())
      labels(data[[1]])
      path_gene <- unique(data[[2]][, .(kegg_paths_name, gene_name, first, second)])
      gene_variant <- unique(data[[2]][, .(gene_name, var_name, second, third)])
      path_gene_tab(path_gene)
      gene_variant_tab(gene_variant)
      
      plot <- create_sankey(labels(), path_gene, gene_variant, data[[3]])
      sankey_plot(plot)
    })
    
    
    observeEvent(event_data("plotly_click", priority = "event"), {
      click_data <- event_data("plotly_click", priority = "event")
      req(click_data)
      
      link_ids <- sankey_link_selection(click_data, path_gene_tab(), gene_variant_tab())
      
    
      link_colors <- rep("lightgray", nrow(labels()))
      link_colors[link_ids - 1] <- "red"

      proxy <- plotlyProxy("sankey_plt", session)
      plotlyProxyInvoke(proxy, "restyle",
                        list(link.color = list(link_colors)))
                    
    })
    
    
    observeEvent(input$sankey_box$maximized, {
      plot_height <- if (input$sankey_box$maximized) {
        "100%"
      } else {
        "50vh"
      }
      
      js_call <- sprintf(
        "
      setTimeout(() => {
        $('#%s').css('height', '%s');
      }, 500)
      $('#%s').trigger('resize');
      ",
      "app-sankey-sankey_plt",
      plot_height,
      "app-sankey-sankey_plt"
      )
      shinyjs::runjs(js_call)
    }, ignoreInit = TRUE)
  })
}


#' sankey_link_selection
#'
#' @param click_data 
#' @param path_gene_tab 
#' @param gene_variant_tab 
#'
#' @return
#' @export
#'
#' @examples
sankey_link_selection <- function(click_data, path_gene_tab, gene_variant_tab) {
  clicked_link <- click_data$pointNumber + 1
  
  if (clicked_link <= nrow(path_gene_tab)) {
    return(first_level_link_selection(clicked_link, path_gene_tab, gene_variant_tab))
  } 
  return(second_level_link_selection(clicked_link, path_gene_tab, gene_variant_tab))
}


#' first_level_link_selection
#'
#' @param clicked_link 
#' @param path_gene_tab 
#' @param gene_variant_tab 
#'
#' @return
#' @export
#'
#' @examples
first_level_link_selection <- function(clicked_link, path_gene_tab, gene_variant_tab) {
  select_gene_name <- path_gene_tab[clicked_link]$gene_name
  first_links <- clicked_link + 1
  second_links <- gene_variant_tab[gene_name == select_gene_name]$third
  if (nrow(unique(path_gene_tab[, .(kegg_paths_name)])) > 1) {
    second_links <- second_links - 1
  }
  link_ids <- unique(c(
    first_links,
    second_links)
  )
  return(link_ids)
}


#' second_level_link_selection
#'
#' @param clicked_link 
#' @param path_gene_tab 
#' @param gene_variant_tab 
#'
#' @return
#' @export
#'
#' @examples
second_level_link_selection <- function(clicked_link, path_gene_tab, gene_variant_tab) {
  select_gene_name <- gene_variant_tab[(clicked_link - nrow(path_gene_tab))]$gene_name
  first_links <- which(path_gene_tab$gene_name == select_gene_name) + 1
  second_links <- gene_variant_tab[gene_name == select_gene_name]$third
  if (nrow(unique(path_gene_tab[, .(kegg_paths_name)])) > 1) {
    second_links <- second_links - 1
  }
  link_ids <- unique(c(
    first_links,
    second_links)
  )
  return(link_ids)
}


