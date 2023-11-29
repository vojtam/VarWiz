

render_genes_table <- function(gene_tab, selected = NULL) {
  print(selected)
  DT::renderDT({
    # gene_tab[, link := paste0(
    #   '<a onmousedown="event.preventDefault(); event.stopPropagation(); return false;" target=_blank href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',
    #   gene_name, '">', '<i class="feature-link fa fa-link"></i>', "</a>"
    # )]
    # gene_tab[, pathways := sprintf(
    #   '<button id="%s" type="button" class="btn action-button btn-secondary" onmousedown="event.preventDefault(); event.stopPropagation(); return false;" onclick="%s">Show</button>', 
    #   gene_name, "Shiny.setInputValue('select_features-gene_pathways_btn', this.id);")]
    
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


render_pathways_table <- function(all_pathways_tab, selected = NULL) {
  print(selected)
  # all_pathways_tab[, genes := sprintf(
  #   '<button id="%s" type="button" class="btn action-button btn-secondary" onmousedown="event.preventDefault(); event.stopPropagation(); return false;" onclick="%s">Show</button>', 
  #   pathway, "Shiny.setInputValue('table-pathway_genes_btn', this.id);")]
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
