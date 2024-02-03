box::use(
  shiny[...],
  bs4Dash[box],
  g3viz[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(
      width = 12,
      status = "navy",
      solidHeader = TRUE,
      align = "center",
      title = "Lollipop plot",
      id = "lollipop_plot_box",
      g3LollipopOutput(ns("lollipop_plot"), height = "700px")
    )
  )
}

#' @export
server <- function(id, data, selected_gene_rval) {
  moduleServer(id, function(input, output, session) {

    output$lollipop_plot <- renderG3Lollipop({
      req(selected_gene_rval())
      filter_tab <- data()[gene_name == selected_gene_rval()]
      plot.options <- g3Lollipop.theme(theme.name = "cbioportal",
                                       title.text = selected_gene_rval(),
                                       y.axis.label = "# of Mutations")

      g3viz::g3Lollipop(
        mutation.dat = filter_tab,
        gene.symbol = selected_gene_rval(),
        gene.symbol.col = "gene_name",
        aa.pos.col = "Protein_position",
        protein.change.col = "HGVSp",
        factor.col = "Consequence",
        btn.style = "gray", # gray-style chart download buttons
        plot.options = plot.options,
      )
    })
  })
}
