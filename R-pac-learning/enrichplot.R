library(clusterProfiler)
library(DOSE)
# details in http://bioconductor.org/packages/devel/bioc/vignettes/enrichplot/inst/doc/enrichplot.html

# 1. genes DE in LUAD FC2, cluster ----------------------------------------

data(geneList, package="DOSE")
de <- names(geneList)[abs(geneList) > 2]
ego <- enrichGO(de, OrgDb = "org.Hs.eg.db", ont="BP", readable=TRUE)

library(enrichplot)
goplot(ego)
barplot(ego, showCategory=20)
dotplot(ego, showCategory=30)
go <- enrichGO(de, OrgDb = "org.Hs.eg.db", ont="all")
library(ggplot2)
dotplot(go, split="ONTOLOGY") + facet_grid(ONTOLOGY~., scale="free")

## remove redundent GO terms
ego2 <- simplify(ego)
cnetplot(ego2, foldChange=geneList)
cnetplot(ego2, foldChange=geneList, circular = TRUE, colorEdge = TRUE)
upsetplot(ego)
heatplot(ego2, foldChange=geneList)
emapplot(ego2)

kk <- gseKEGG(geneList, nPerm=10000)
ridgeplot(kk)
