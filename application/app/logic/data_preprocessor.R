
box::use(
  data.table[...],
)


#' export
preprocess_data <- function() {
  data_dir <- file.path("data/")
  
  variants_tab_full <- fread(file.path(data_dir, "all_variants.annotated.processed.tsv"))
  kegg_tab <- fread(file.path(data_dir, "kegg_tab.tsv"))
  
  
  variants_tab <- variants_tab_full[, .(Consequence, GIVEN_REF, Allele, Gene, BIOTYPE, SOURCE, SYMBOL, VARIANT_CLASS, chrom, pos)]
  variants_tab <- variants_tab[SOURCE == "Ensembl"]
  
  setnames(variants_tab, old = "Gene", new = "ensembl_id")
  
  ## join the tables
  tab <- variants_tab[kegg_tab, on = "ensembl_id", nomatch = 0]
  tab[, gene_definition := NULL]
  return(tab)
}
