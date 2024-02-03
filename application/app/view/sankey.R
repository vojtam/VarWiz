box::use(
  shiny[moduleServer, observe, reactive, br, div, verbatimTextOutput, renderPrint, isolate, NS, req, validate, need, tagList, renderPlot, plotOutput, observeEvent, reactiveVal, reactiveValues],
  plotly[plot_ly, event_data, plotlyOutput, renderPlotly, plotlyProxy, plotlyProxyInvoke],
  data.table[...],
  purrr[map2],
  bs4Dash[box],
  graphics[legend, mtext],
  stats[na.omit],
  shinyjs[...],
  gargoyle[...]
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
      height = "54vh",
      title = "Sankey Diagram",
      status = "navy",
      maximizable = TRUE,
      solidHeader = TRUE,
      id = ns("sankey_box"),
      div(id = "legend"),
      plotlyOutput(ns("sankey_plt"), height = "50vh")
      #plotOutput(ns("sankey_legend"))
    )
  )
}

#' @export
server <- function(id, dataset, selected_pathways_react, selected_genes_react, is_path_tab_active) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    # reactive value holding the sankey plot

# reactiveVal initialization ----------------------------------------------

    sankey_plot <- reactiveVal()
    sankey_pathway_plot <- reactiveVal()
    sankey_gene_plot <- reactiveVal()
    labels <- reactiveVal()
    path_gene_tab <- reactiveVal()
    gene_variant_tab <- reactiveVal()
    selected_gene_name <- reactiveVal()
    

# Render ------------------------------------------------------------------

    output$sankey_plt <- renderPlotly({
      
      validate(
        need(!((is.null(selected_pathways_react()) || nrow(selected_pathways_react()) == 0)
               && (is.null(selected_genes_react()) || nrow(selected_genes_react()) == 0)),
             "select the features (pathways / genes) that you want to visualize from the table on the right")
      )
      
      legend_tab <- unique(dataset()[, .(Variant_Type, col)])
      prepare_legend_r()
      map2(.x = legend_tab$Variant_Type, .y = legend_tab$col, \(title, color) add_to_legend_r(title, color))

      # output the contents of the sankey_plot reactive value
      if (is_path_tab_active()) {
        sankey_pathway_plot()
      } else {
        sankey_gene_plot()
      }
    })
    
    
    

# Events ------------------------------------------------------------------

    
    on("reset_sankey", {
      if (is_path_tab_active()) {
        sankey_pathway_plot(NULL)
      } else {
        sankey_gene_plot(NULL)
      }
    }) 
    
    # data is passed into module -> create sankey
    observe({
      
      if (is_path_tab_active()) {
        req(selected_pathways_react())
      }
      else {
        req(selected_genes_react())
      }
      
      data <- sankey_prepare_data(dataset(), selected_pathways_react(), selected_genes_react(), is_path_tab_active())
      labels(data[[1]])
      path_gene <- unique(data[[2]][, .(kegg_paths_name, gene_name, first, second)])
      gene_variant <- unique(data[[2]][, .(gene_name, var_name, col, second, third)])
      setorder(path_gene, first)
      setorder(gene_variant, second)
      path_gene_tab(path_gene)
      gene_variant_tab(gene_variant)
      
      
      colors <- NULL
      if (nrow(labels()) > 0) {
        colors <- c(rep("lightgray", nrow(path_gene)), gene_variant$col) 

      }
      plot <- create_sankey(labels(), path_gene, gene_variant, data[[3]], colors)
      
      if (is_path_tab_active()) {
        sankey_pathway_plot(plot)
      }
      else {
        sankey_gene_plot(plot)
      }
    })
    

    
    
    observeEvent(event_data("plotly_click", priority = "event"), {
      click_data <- event_data("plotly_click", priority = "event")
      req(click_data)
      
      result <- sankey_link_selection(click_data, path_gene_tab(), gene_variant_tab())
      link_ids <- result[[1]]
      gene_name <- result[[2]]
      selected_gene_name(gene_name)
    
      link_colors <- rep("lightgray", nrow(labels()))
      link_colors[link_ids] <- "red"
      
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
    
    return(selected_gene_name)
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
  labels <- data.table(label = c(path_gene_tab$kegg_paths_name, gene_variant_tab$gene_name), id = c(path_gene_tab$first, gene_variant_tab$second))
  first_links <- clicked_link

  second_links <- which(labels$label == select_gene_name)
  link_ids <- unique(c(
    first_links,
    second_links)
  )
  return(list(link_ids, select_gene_name))
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
  labels <- data.table(label = c(path_gene_tab$kegg_paths_name, gene_variant_tab$gene_name), id = c(path_gene_tab$first, gene_variant_tab$second))
  
  first_links <- which(path_gene_tab$gene_name == select_gene_name) 
  second_links <- which(labels$label == select_gene_name)

  link_ids <- unique(c(
    first_links,
    second_links)
  )
  return(list(link_ids, select_gene_name))
}

prepare_legend_r <- function() {
  js_call <- "
    let legend_div = document.getElementById('legend');
    legend_div.textContent = '';
  "
  shinyjs::runjs(js_call)
}

add_to_legend_r <- function(title, color) {
  js_call <- sprintf(
    "

	  let legend_div = document.getElementById('legend');
	
    let legend_item = document.createElement('div');
    let col_circle = document.createElement('div');
    let legend_span = document.createElement('span');

    legend_item.classList.add('legend-item');
    col_circle.classList.add('legend-col-circle');
    legend_span.classList.add('legend-span');

    legend_span.textContent = '%s';
    col_circle.style.backgroundColor = '%s';

    legend_item.append(col_circle);
    legend_item.append(legend_span);

    legend_div.append(legend_item);
    
    
    console.log('DONE');
    "
  , title, color)
  shinyjs::runjs(js_call)
}
