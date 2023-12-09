
box::use(
  data.table[...],
)


#' preprocess_data
#' @description
#' This function loads the datasets tsvs, merges them and filters the columns
#' 
#'
#' @return table with the following columns:
#' Consequence | given_ref | Allele | var_name | ensembl_id | biotype |
#' gene_name | variant_class | chrom | pos | all_kegg_gene_names | refseq_id |
#' kegg_paths_id | kegg_paths_name
#' @export
#'
#' @examples
preprocess_data <- function() {
  data_dir <- file.path("data/")
  
  variants_tab_full <- fread(file.path(data_dir, "all_variants.annotated.processed.tsv"))
  kegg_tab <- fread(file.path(data_dir, "kegg_tab.tsv"))
  
  
  variants_tab <- variants_tab_full[, .(Consequence, GIVEN_REF, Allele, var_name, Gene, BIOTYPE, SOURCE, SYMBOL, VARIANT_CLASS, chrom, pos)]
  variants_tab <- variants_tab[SOURCE == "Ensembl"]
  
  setnames(variants_tab,
           old = c("Gene", "GIVEN_REF", "BIOTYPE", "SYMBOL", "VARIANT_CLASS"),
           new = c("ensembl_id", "given_ref", "biotype", "gene_name", "variant_class"))
  
  ## join the tables
  tab <- variants_tab[kegg_tab, on = "ensembl_id", nomatch = 0]
  tab[, `:=`(gene_definition = NULL, SOURCE = NULL, type = NULL)]
  return(tab)
}



