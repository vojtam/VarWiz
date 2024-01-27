
box::use(
  data.table[...],
  stats[na.omit]
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
  
  variants_tab_full <- fread(file.path(data_dir, "all_variants.annotated.processed.tsv"), na.strings = c(".", "-"))
  kegg_tab <- fread(file.path(data_dir, "kegg_tab.tsv"))
  
  variants_tab <- variants_tab_full[, .(Consequence, GIVEN_REF, Allele, Protein_position, HGVSp, Amino_acids, var_name, Gene, BIOTYPE, SOURCE, SYMBOL, VARIANT_CLASS, chrom, pos)]
  variants_tab <- variants_tab[!is.na(variants_tab$HGVSp)]
  variants_tab <- variants_tab[SOURCE == "Ensembl"]
  
  setnames(variants_tab,
           old = c("Gene", "GIVEN_REF", "BIOTYPE", "SYMBOL", "VARIANT_CLASS", "Consequence", "Protein_position", "Amino_acids"),
           new = c("ensembl_id", "given_ref", "biotype", "gene_name", "variant_class", "consequence", "protein_pos", "amino_acids"))
  
  ## join the tables
  tab <- variants_tab[kegg_tab, on = "ensembl_id", nomatch = 0]
  tab[, `:=`(gene_definition = NULL, SOURCE = NULL, type = NULL)]
  return(tab)
}




#' preprocess_maf_data
#' @description
#' This function loads the datasets tsvs, merges them and filters the columns
#' 
#'
#' @return table 
#' @export
#'
#' @examples
preprocess_maf_data <- function() {
  data_dir <- file.path("data/")
  kegg_tab <- fread(file.path(data_dir, "kegg_tab.tsv"))[, .(ensembl_id, kegg_paths_name)]
  kegg_tab <- kegg_tab[ensembl_id != ""]
  mutations_data <- fread(file.path(data_dir, "brca_tcga_pub2015/data_mutations.txt"), sep = "\t")
  
  variants_tab <- mutations_data[, .(Hugo_Symbol, Gene, Chromosome, Start_Position, End_Position, Strand, Consequence, Variant_Classification, Variant_Type, Reference_Allele, Tumor_Seq_Allele2, HGVSc, HGVSp, Protein_position, Codons)]
  setnames(variants_tab,
           old = c("Gene", "Hugo_Symbol", "Tumor_Seq_Allele2"),
           new = c("ensembl_id", "gene_name", "Allele")
  )
  
  variants_tab <- na.omit(variants_tab)
  genes_sample <- sample(variants_tab$gene_name, 2000)
  variants_tab <- variants_tab[gene_name %in% genes_sample]
  variants_tab[, var_name := paste0(Chromosome, "_", Start_Position, "_", Reference_Allele, "/", Allele)]
  tab <- variants_tab[kegg_tab, on = "ensembl_id", nomatch = 0, allow.cartesian = TRUE]
  return(tab)
  
}
