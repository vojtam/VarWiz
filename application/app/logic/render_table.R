

#' render_genes_table
#'
#' @param all_pathways_tab a data table with a single column e.g.
#'  | gene_name |
#'    RPL22
#' @param selected a (1-based) vector of indices of selected genes
#' - used if we have been using pathways table and now we have switched back to
#'  genes tab and want to rerender the table, keeping the genes that we selected
#'  previously
#'
#' @return rendered DT table 
#' @export
#'
#' @examples
render_genes_table <- function(gene_tab, selected = NULL) {
  print(selected)
  DT::renderDT({
   
    DT::datatable(
      gene_tab,
      escape = FALSE,
      selection = list(
        mode = "multiple",
        selected = selected,
        target = "row"
      ),
      options = list(
        pageLength = 15
      )
    )
  })
}


#' render_pathways_table
#'
#' @param all_pathways_tab a data table with a single column e.g.
#'  | pathway |
#'    Covid-19
#' @param selected a (1-based) vector of indices of selected pathways
#' - used if we have been using gene table and now we have switched back
#'  to pathways and want to rerender the table, keeping the pathways that
#'  we selected previously
#'
#' @return rendered DT table 
#' @export
#'
#' @examples
render_pathways_table <- function(all_pathways_tab, selected = NULL) {
  print(selected)
  DT::renderDT({
    tab <- DT::datatable(
      all_pathways_tab,
      escape = FALSE,
      selection = list(
        mode = "multiple",
        selected = selected,
        target = "row"
      ),
      callback = htmlwidgets::JS('
                                  table.on("mouseover", "td", function() {
                                  var index = table.cell(this).index();
                                  Shiny.setInputValue("table-hover_pathway_genes", index, {priority: "event"});
                                   })'),
      options = list(pageLength = 15)
    )

    return(tab)
  })
}
