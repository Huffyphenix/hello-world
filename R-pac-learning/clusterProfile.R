library(org.Hs.eg.db)
data(geneList, package="DOSE")
gene <- names(geneList)[abs(geneList) > 2]
gene.df <- bitr(gene, fromType = "ENTREZID",
                toType = c("ENSEMBL", "SYMBOL"),
                OrgDb = org.Hs.eg.db)
head(gene.df)


# Go analysis -------------------------------------------------------------

ggo <- groupGO(gene     = gene, # a vector of gene IDs (can be any ID type that supported by corresponding  OrgDb).
               OrgDb    = org.Hs.eg.db,
               ont      = "CC", # one of MF, BP and CC
               level    = 3, # one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
               readable = TRUE) # If readable is setting to TRUE, the input gene IDs will be converted to gene symbols.
head(ggo)

ego <- enrichGO(gene          = gene,
                universe      = names(geneList),
                OrgDb         = org.Hs.eg.db,
                ont           = "CC", 
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
                readable      = TRUE)
head(ego)

ego2 <- enrichGO(gene         = gene.df$ENSEMBL,
                 OrgDb         = org.Hs.eg.db,
                 keyType       = "ENSEMBL",
                 ont           = "CC",
                 pAdjustMethod = "BH",
                 pvalueCutoff  = 0.01,
                 qvalueCutoff  = 0.05)

ego2 <- setReadable(ego2, OrgDb = org.Hs.eg.db) # Gene ID can be mapped to gene Symbol by using paramter readable=TRUE or setReadable function.
head(ego2)

# reduce redundancy of enriched GO terms ----------------------------------

enrichMap(ego2)
ego2_simp <- simplify(ego2, cutoff=0.7, by="p.adjust", select_fun=min)
enrichMap(ego2_simp)

# GO Gene Set Enrichment Analysis -----------------------------------------

ego3 <- gseGO(geneList     = geneList,
              OrgDb        = org.Hs.eg.db,
              ont          = "CC",
              nPerm        = 1000,
              minGSSize    = 100,
              maxGSSize    = 500,
              pvalueCutoff = 0.05,
              verbose      = FALSE)
head(ego3)


# KEGG analysis -----------------------------------------------------------

search_kegg_organism('ece', by='kegg_code') #help searching supported organisms.

kk <- enrichKEGG(gene         = gene,
                 organism     = 'hsa',
                 pvalueCutoff = 0.05)
head(kk)

# KEGG Gene Set Enrichment Analysis ---------------------------------------


kk2 <- gseKEGG(geneList     = geneList,
               organism     = 'hsa',
               nPerm        = 1000,
               minGSSize    = 120,
               pvalueCutoff = 0.05,
               verbose      = FALSE)
head(kk2)

# KEGG Module over-representation test
# KEGG Module is a collection of manually defined function units. In some situation, KEGG Modules have a more straightforward interpretation.

mkk <- enrichMKEGG(gene = gene,
                   organism = 'hsa')
head(mkk)

# KEGG Module Gene Set Enrichment Analysis
mkk2 <- gseMKEGG(geneList = geneList,
                 organism = 'hsa')
head(mkk2)
