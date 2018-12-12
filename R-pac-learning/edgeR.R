data_path <- "/project/huff/huff/TCGA_AML/siSTAB1_data/RNAseq-analysis/result/2.Analysis_result/Single_Sample"

fileNames <- dir(data_path,pattern = "count$")


# load expression data ----------------------------------------------------

for (x in fileNames) {
  assign(x,readr::read_tsv(file.path(data_path,x),col_names = F))
}

# data manage -------------------------------------------------------------
NC1.count %>% dplyr::rename("ENSG"="X1","NC1"="X2") -> NC1_count.comfirm
NC2.count  %>% dplyr::rename("ENSG"="X1","NC2"="X2")-> NC2_count.comfirm
siSTAB1_1.count  %>% dplyr::rename("ENSG"="X1","siSTAB1_1"="X2") -> siSTAB1_1_count.comfirm
siSTAB1_2.count  %>% dplyr::rename("ENSG"="X1","siSTAB1_2"="X2") -> siSTAB1_2_count.comfirm

NC1_count.comfirm %>%
  dplyr::inner_join(NC2_count.comfirm,by=c("ENSG")) %>%
  dplyr::inner_join(siSTAB1_1_count.comfirm,by=c("ENSG")) %>%
  dplyr::inner_join(siSTAB1_2_count.comfirm,by=c("ENSG")) -> all_count


# check data distribution -------------------------------------------------------
library(CancerSubtypes)
mRNA <- as.matrix(all_count[,-c(1)])
rownames(mRNA)=all_count$ENSG
###To observe the mean, variance and Median Absolute Deviation distribution of the dataset, it helps users to get the distribution characteristics of the data, e.g. To evaluate whether the dataset fits a normal distribution or not.
data.checkDistribution(mRNA) # data don't fit a nomal distribution


# do edgeR to get DEGs ----------------------------------------------------
# Userguide: http://www.bioconductor.org/packages/3.6/bioc/vignettes/edgeR/inst/doc/edgeRUsersGuide.pdf

setwd("/project/huff/huff/TCGA_AML/siSTAB1_data/DEG")
## get DEGlist
d <- DGEList(counts=mRNA,group=c(1,1,2,2)) # construct a DEGList for edgeR analysis
d_backup <- d

keep <- rowSums(cpm(d)>100) >= 2 # filtering genes with very low counts across all libraries provide little evidence for differential expression
d <- d[keep,]
d$samples$lib.size <- colSums(d$counts) # recalculate the library size
d <- calcNormFactors(d) # The calcNormFactors function normalizes for RNA composition by finding a set of scaling factors for the library sizes that minimize the log-fold changes between the samples for most genes.
d$samples #The normalization factors of all the libraries multiply to unity. A normalization factor below 1 indicates that a small number of high count genes are monopolizing the sequencing, causing the counts for other genes to be lower than would be usual given the library size. As a result, the library size will be scaled down, analogous to scaling the counts upwards in that library. Conversely, a factor above 1 scales up the library size, analogous to downscaling the counts.
plotMDS(d, method="bcv", col=as.numeric(d$samples$group)) # Multidimensional scaling plot of distances between gene expression profiles

d1 <- estimateDisp(d) # To estimate common dispersion and tagwise dispersions in one run

plotBCV(d1)

et <- exactTest(d1) # Testing for DE genes
topTags(et)

de1 <- decideTestsDGE(et, adjust.method="BH", p.value=0.05) 
de1tags12 <- rownames(d1)[as.logical(de1)] 
plotSmear(et, de.tags=de1tags12)
dev.off()

nrDEG=topTags(et, n=nrow(mRNA))
nrDEG=as.data.frame(nrDEG)

library(org.Hs.eg.db)
library(clusterProfiler)
bitr(rownames(nrDEG), fromType = "ENSEMBL",
                         toType = c("SYMBOL"),
                         OrgDb = org.Hs.eg.db) -> ensembl.symbol

setwd("/project/huff/huff/TCGA_AML/siSTAB1_data/DEG")
nrDEG %>%
  tibble::as.tibble() %>%
  dplyr::mutate(ENSEMBL=rownames(nrDEG)) %>%
  dplyr::left_join(ensembl.symbol,by="ENSEMBL") %>%
  readr::write_tsv(file.path("edger_classic.results.tsv"))

DEG_edger_classic(mRNA,c(1,1,2,2))
## unit edgeR into a function
DEG_edger_classic <- function(exprSet=exprSet,group_list=group_list){
  d <- DGEList(counts=exprSet,group=factor(group_list))
  d.full <- d # keep the old one in case we mess up
  #apply(d$counts, 2, sum) # total gene counts per samplekeep <- rowSums(cpm(d)>100) >= 2
  keep <- rowSums(cpm(d)>100) >= 2
  d <- d[keep,]
  d$samples$lib.size <- colSums(d$counts)
  d <- calcNormFactors(d)
  d$samples 
  png("MDS.png")
  plotMDS(d, method="bcv", col=as.numeric(d$samples$group)) # Multidimensional scaling plot of distances between gene expression profiles
  legend("bottomleft", as.character(unique(d$samples$group)), col=1:3, pch=20)
  dev.off()
  
  d1 <- estimateCommonDisp(d1, verbose=T)
  d1 <- estimateTagwiseDisp(d1)
  
  png("BCV.png")
  plotBCV(d1)
  dev.off()
  
  et12 <- exactTest(d1) # Testing for DE genes
  
  png("MA.png")
  de1 <- decideTestsDGE(et12, adjust.method="BH", p.value=0.05) 
  de1tags12 <- rownames(d1)[as.logical(de1)] 
  plotSmear(et12, de.tags=de1tags12)
  dev.off()
  
  nrDEG=topTags(et12, n=nrow(exprSet))
  nrDEG=as.data.frame(nrDEG)
  write.table(nrDEG,"edger_classic.results.csv",sep = "\t")
}


# confirm with expression data --------------------------------------------


fileNames <- dir("/project/huff/huff/TCGA_AML/siSTAB1_data/expression",pattern = "RPKM$")
# load expression data ----------------------------------------------------
fn_exp_manage <- function(x){
  x %>%
    dplyr::rename("ENSG_id"="X1","symbol" = "X2", "FPKM" = "X3") %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(FPKM_sum = sum(FPKM)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ENSG_id,symbol,FPKM_sum) %>%
    unique()
}
for (x in fileNames) {
  assign(x,readr::read_tsv(file.path("/project/huff/huff/TCGA_AML/siSTAB1_data/expression",x),col_names = F))
}
fn_exp_manage(NC1.RPKM) %>% dplyr::rename("NC1" ="FPKM_sum") -> NC1_exp.comfirm
fn_exp_manage(NC2.RPKM)  %>% dplyr::rename("NC2" ="FPKM_sum")-> NC2_exp.comfirm
fn_exp_manage(siSTAB1_1.RPKM)  %>% dplyr::rename("siSTAB1_1" ="FPKM_sum") -> siSTAB1_1_exp.comfirm
fn_exp_manage(siSTAB1_2.RPKM)  %>% dplyr::rename("siSTAB1_2" ="FPKM_sum") -> siSTAB1_2_exp.comfirm


NC1_exp.comfirm %>%
  dplyr::inner_join(NC2_exp.comfirm,by=c("symbol","ENSG_id")) %>%
  dplyr::inner_join(siSTAB1_1_exp.comfirm,by=c("symbol","ENSG_id")) %>%
  dplyr::inner_join(siSTAB1_2_exp.comfirm,by=c("symbol","ENSG_id")) -> all_exp

all_exp %>%
  readr::write_tsv("RPKM_expression.tsv")
fn_ttest <- function(x){
  x %>% as.matrix() %>% .[1,c(1,2)] %>% as.vector() -> nc
  x %>% as.matrix() %>% .[1,c(3,4)] %>% as.vector() -> si
  t.test(nc,si) %>%
    broom::tidy()
}
all_exp %>%
  tidyr::nest(-ENSG_id,-symbol) %>%
  dplyr::group_by(ENSG_id,symbol) %>%
  dplyr::mutate(ttest = purrr::map(data,fn_ttest)) -> exp_DE

exp_DE %>%
  dplyr::select(-data) %>%
  tidyr::unnest() %>%
  dplyr::rename("nc" = "estimate1","si"="estimate2") %>%
  dplyr::mutate(log2FC = log2(si/nc)) %>%
  dplyr::filter(p.value <= 0.05) -> exp_DE.p0.05

exp_DE.p0.05 %>%
  readr::write_tsv("ttest_rpkm_exp_DE.p0.05")
