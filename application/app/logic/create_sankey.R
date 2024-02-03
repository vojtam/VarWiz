
box::use(
  data.table[...],
  stats[na.omit],
  plotly[plot_ly, layout, add_annotations],
)

#' sankey_preprare_data
#' @description
#' The purpose of this function is to wrangle the data to such a form that
#' it plotly sankey can be made from it. 
#' 
#'
#' @param dataset A data table coming from preprocess_data function, it has the
#' following columns:
#'  Consequence | given_ref | Allele | var_name | ensembl_id | biotype |
#'  gene_name | variant_class | chrom | pos | all_kegg_gene_names | refseq_id |
#'  kegg_paths_id | kegg_paths_name
#' @param selected_pathways a data.table of selected pathways for filtering.
#'  The table has having the following structure:
#'        
#'    | pathway | row_i |
#     1: Endocytosis     5
#'
#' @return list of two data.tables and one vector:
#' - labels is a table with two columns:
#' | label | id |
#' COVID-19 | 1 |
#' 
#' - full has the following columns:
#' gene_name | kegg_paths_name | var_name | first | second | third
#' 
#' - scores is a vector of values
#' @export
#'
#' @examples
sankey_prepare_data <- function(dataset, selected_pathways, selected_genes, is_pathway) {
  # filter the selected_pathways -> | gene_name | kegg_paths_name |
  if (!is_pathway) {
    sankey_data <- dataset[gene_name %in% selected_genes$gene_name, .(gene_name, kegg_paths_name, var_name, col)]
  } else {
    sankey_data <- dataset[kegg_paths_name %in% selected_pathways$pathway, .(gene_name, kegg_paths_name, var_name, col)]
  }

  setorder(sankey_data, kegg_paths_name, gene_name)
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
  path_gene <- unique(full[, .(kegg_paths_name, gene_name, first, second)])
  scores <- c(rle(rleid(c(full$first + full$second)))$lengths, rep(1, nrow(full)))
  scores <- rep(1, length(scores))
  return(list(labels, full, scores))
}



#' Title
#'
#' @param labels 
#' @param sankey_data 
#'
#' @return
#' @export
#'
#' @examples
create_sankey <- function(labels, path_gene, gene_variant, scores, colors) {
  path_gene_labels <- labels
  path_gene_labels[(nrow(labels) - length(unique(gene_variant$var_name)) + 1):nrow(labels)]$label <- ""
  
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    selectedpoints = c(0:10),
    node = list(
      label = path_gene_labels$label,
      y = seq(0, nrow(labels), by = 1),
      color = "black",
      pad = 30,
      customdata = labels$label,
      hovertemplate = paste("%{customdata}"),
      thickness = 30,
      line = list(
        color = "black",
        width = 2
      )
    ),
    link = list(
      source = c(path_gene$first, gene_variant$second) - 1,
      target = c(path_gene$second, gene_variant$third) - 1,
      value = scores,
      color = colors
    )
  )


  
  fig <- fig |> add_annotations(x = 0, y = -0.1, showarrow = FALSE, font = list(size = 17), xref = "x", yref = "paper", text = "Pathway")
  fig <- fig |> add_annotations(x = 1, y = -0.1, showarrow = FALSE, font = list(size = 17), xref = "x", yref = "paper", text = "Gene")
  fig <- fig |> add_annotations(x = 2, y = -0.1, showarrow = FALSE, font = list(size = 17), xref = "x", yref = "paper", text = "Variant")
  
  fig <- fig |> layout(
    font = list(
      size = 14
    ),
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      visible = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      visible = FALSE
    )
  )
  fig
}
