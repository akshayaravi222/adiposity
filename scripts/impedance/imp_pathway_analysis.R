library(data.table)
library(pbapply)
library(dplyr)
library(tidyr)
library(stringr)
# Libraries for pathway analysis
library(ggridges) 
library(org.Hs.eg.db)
library(biomaRt)
library(ReactomePA)
library(DOSE)
library(clusterProfiler)
library(enrichplot)

results <- read.csv('/n/groups/patel/akshaya/01_ukb/out/batch_imp_proteome_combined.csv', header=T)
nrow(results)

results <- results %>%
  filter(Adiposity != "Adiposity")
head(results)

results$estimate <- as.numeric(results$estimate)
results$sterror <- as.numeric(results$sterror)
results$z_value <- as.numeric(results$z_value)
results$p_value <- as.numeric(results$p_value)
results$n <- as.numeric(results$n)

# id <- 'trunk_tissue_fat_percentage_f23286_2_0'


# nrow(results)

# length(unique(names(GeneLists$GSEA)))
# names(GeneLists$GSEA)[duplicated(names(GeneLists$GSEA))]

# Function to build genelist
CreateGenelists <- function(id){
  GeneLists <- list()
  
  adip_assoc <- results %>% 
    filter(Adiposity == id) #MUST not overlap id and Eid naming.
  
  # USE the t value: Contains significance and direction. Instead of Beta Estimate.
  GSEAgenelist <- adip_assoc$z_value
  names(GSEAgenelist) <- adip_assoc$Protein
  
  #ONLY keep the first name of set of complexes || to prevent ties in GSEA.
  #names(GSEAgenelist) <- gsub("_.*", "", names(GSEAgenelist)) 
  GSEAgenelist = sort(GSEAgenelist, decreasing = TRUE)
  
  GeneLists[["GSEA"]] <- GSEAgenelist
  
  return(GeneLists)
}

# Function to Run GSEA Analysis
PSEA_tissue <- function(GeneList){
  GTEX <- fread("/n/groups/patel/shakson_ukb/UK_Biobank/Data/GeneSets/GTEX_tissue.txt")
  omicpredIDs <- fread(file = "/n/groups/patel/shakson_ukb/UK_Biobank/Data/OMICSPRED/UKB_Olink_multi_ancestry_models_val_results_portal.csv")
  UKB_universe = unlist(strsplit(omicpredIDs$Gene, "_"))
  
  gsea <- GSEA(geneList= GeneList[["GSEA"]], 
               minGSSize = 10, 
               maxGSSize = 500, 
               pvalueCutoff = 0.05, 
               pAdjustMethod = "BH", 
               TERM2GENE=GTEX, 
               TERM2NAME=NA)
  return(gsea@result)
}

# Function: Obtain Olink Protein Names to Entrez IDs map
entrezMap <- function(){
  # OMIC IDs
  omicpredIDs <- fread(file = "/n/groups/patel/shakson_ukb/UK_Biobank/Data/OMICSPRED/UKB_Olink_multi_ancestry_models_val_results_portal.csv")
  
  # Connect to the Ensembl database
  ensembl = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  
  # Convert UniProt IDs to Entrez IDs
  map_prot_to_entrez <- function(uniprot_ids) {
    converted_ids <- getBM(attributes = c("hgnc_symbol", "entrezgene_id"),
                           filters = "hgnc_symbol",
                           values = uniprot_ids,
                           mart = ensembl)
    return(converted_ids)
  }
  
  # Split the protein names into separate genes - if needed:
  df_split <- omicpredIDs %>%
    mutate(genes = str_split(Gene, "_")) %>%
    unnest(genes)
  
  # Apply the mapping function to each gene
  entrez_results = map_prot_to_entrez(df_split$genes)
  
  # Combine results into a single data frame
  df_entrez <- df_split %>%
    left_join(entrez_results, by = c("genes" = "hgnc_symbol"))
  
  # Identify UniProt IDs that have not been converted
  OlinkIDs_noconvert <- df_entrez[is.na(df_entrez$entrezgene_id),]$Gene
  
  return(df_entrez)
}

# Function: Convert Genelists to include Entrez IDs
convertEntrez <- function(GeneList){
  
  df_entrez <- entrezMap()
  
  for(i in names(GeneList)){
    name <- paste0(i,"_entrez")
    if(is.null(names(GeneList[[i]]))){ #If not a named character (ORA)
      GeneList[[name]] <- df_entrez[match(GeneList[[i]], 
                                          df_entrez$Gene),]$entrezgene_id
    }else{
      GeneList[[name]] <- GeneList[[i]]
      names(GeneList[[name]]) <- df_entrez[match(names(GeneList[[i]]),
                                                 df_entrez$Gene),]$entrezgene_id
    }
  }
  
  return(GeneList)
}

# Function: GSEA of Pathways (Uses Entrez ID)
PSEA_paths <- function(GeneList){
  EnrichGSEA <- list()
  
  Reactome <- gsePathway(GeneList[["GSEA_entrez"]], 
                         minGSSize = 10, maxGSSize = 500,
                         pvalueCutoff = 0.05, pAdjustMethod = "BH", 
                         verbose = TRUE,
                         eps = 0)
  
  return(Reactome@result)
}

# Tissue + Reactome: GSEA:
TissueEnrichGSEA <- function(Aid){
  tissue <- list()
  
  PA_SS <- CreateGenelists(id = Aid)
  PSEA_res <- PSEA_tissue(PA_SS)
  
  return(PSEA_res)
}
PathwayEnrichGSEA <- function(Aid){
  pathways <- list()
  
  PA_SS <- CreateGenelists(id = Aid)
  PA_SS_entrez <- convertEntrez(PA_SS)
  PA_SS_entrez$GSEA_entrez <- PA_SS_entrez$GSEA_entrez[!is.na(names(PA_SS_entrez$GSEA_entrez))]
  
  GSEA_enrich <- PSEA_paths(PA_SS_entrez)
  
  return(GSEA_enrich)
}

df_entrez <- entrezMap()


# Run tissue and pathway enrichment across all exposures that had a signficant hit:
#Use maximal specification only:
adiposity <- sort(unique(results$Adiposity))

#Run Tissue Enrichments:
tissue <- pblapply(adiposity, function(x) {
  Tgsea <- suppressMessages(suppressWarnings(
    TissueEnrichGSEA(Aid = x)
  ))
  return(Tgsea)
})
names(tissue) <- adiposity

#Run Pathway Enrichments:
pathway <- pblapply(adiposity, function(x) {
  Pgsea <- suppressMessages(suppressWarnings(
    PathwayEnrichGSEA(Aid = x)
  ))
  return(Pgsea)
})
names(pathway) <- adiposity

saveRDS(tissue, file='/n/groups/patel/akshaya/01_ukb/out/bmi_proteome_tissues.rds')

# Convert the pathway core_enrichment from the Entrez IDs to names
# 1. pull out every unique Entrez ID
all.ids <- unique(unlist(
  strsplit(
    unlist(lapply(pathway, `[[`, "core_enrichment")),
    "/"
  )
))

# 2. map Entrez → SYMBOL
id2sym <- AnnotationDbi::select(
  org.Hs.eg.db,
  keys      = all.ids,
  columns   = c("SYMBOL"),
  keytype   = "ENTREZID"
)
# make a named vector for fast lookup
sym.vec <- setNames(id2sym$SYMBOL, id2sym$ENTREZID)

# 3. write a helper that converts one df’s core_enrichment column
convert_core <- function(df) {
  df$core_enrichment <- vapply(
    df$core_enrichment,
    function(x) {
      ids   <- strsplit(x, "/", fixed=TRUE)[[1]]
      syms  <- sym.vec[ids]
      # if any NA (unmapped), keep the original ID in place
      syms[is.na(syms)] <- ids[is.na(syms)]
      paste(syms, collapse = "/")
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  df
}

# 4. apply over your list
gsea_list_named <- lapply(pathway2, convert_core)
saveRDS(gsea_list_named, file='/n/groups/patel/akshaya/01_ukb/out/bmi_proteome_pathways.rds')

