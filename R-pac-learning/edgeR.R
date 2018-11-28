data_path <- "/project/huff/huff/TCGA_AML/siSTAB1_data/RNAseq-analysis/result/2.Analysis_result/Single_Sample"

fileNames <- dir(data_path,pattern = "count$")


# load expression data ----------------------------------------------------

for (x in fileNames) {
  assign(x,readr::read_tsv(file.path(data_path,x),col_names = F))
}

# data manage -------------------------------------------------------------
fn_exp_manage <- function(x){
  x %>%
    dplyr::rename("ENSG_id"="X1","symbol" = "X2", "FPKM" = "X3") %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(FPKM_sum = sum(FPKM)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ENSG_id,symbol,FPKM_sum) %>%
    unique()
}

fn_exp_manage(NC1.RPKM) %>% dplyr::rename("NC1" ="FPKM_sum") -> NC1_exp.comfirm
fn_exp_manage(NC2.RPKM)  %>% dplyr::rename("NC2" ="FPKM_sum")-> NC2_exp.comfirm
fn_exp_manage(siSTAB1_1.RPKM)  %>% dplyr::rename("siSTAB1_1" ="FPKM_sum") -> siSTAB1_1_exp.comfirm
fn_exp_manage(siSTAB1_2.RPKM)  %>% dplyr::rename("siSTAB1_2" ="FPKM_sum") -> siSTAB1_2_exp.comfirm

NC1_exp.comfirm %>%
  dplyr::inner_join(NC2_exp.comfirm,by=c("symbol","ENSG_id")) %>%
  dplyr::inner_join(siSTAB1_1_exp.comfirm,by=c("symbol","ENSG_id")) %>%
  dplyr::inner_join(siSTAB1_2_exp.comfirm,by=c("symbol","ENSG_id")) -> all_exp


# data distribution -------------------------------------------------------
library(CancerSubtypes)
mRNA <- as.matrix(all_exp[,-c(1,2)])
rownames(mRNA)=all_exp$symbol
###To observe the mean, variance and Median Absolute Deviation distribution of the dataset, it helps users to get the distribution characteristics of the data, e.g. To evaluate whether the dataset fits a normal distribution or not.
data.checkDistribution(mRNA) # data don't fit a nomal distribution


# do edgeR to get DEGs ----------------------------------------------------


d <- DGEList(counts=mRNA,group=c(1,1,2,2)) # construct a DEGList for edgeR analysis


DEG_edger_classic <- function(exprSet=exprSet,group_list=group_list){
  d <- DGEList(counts=exprSet,group=factor(group_list))
  d.full <- d # keep the old one in case we mess up
  #apply(d$counts, 2, sum) # total gene counts per samplekeep <- rowSums(cpm(d)>100) >= 2
  keep <- rowSums(cpm(d)>100) >= 2
  d <- d[keep,]
  d$samples$lib.size <- colSums(d$counts)
  d <- calcNormFactors(d)
  
  png("MDS.png")
  plotMDS(d, method="bcv", col=as.numeric(d$samples$group))
  legend("bottomleft", as.character(unique(d$samples$group)), col=1:3, pch=20)
  dev.off()
  
  d1 <- estimateCommonDisp(d, verbose=T)
  d1 <- estimateTagwiseDisp(d1)
  
  png("BCV.png")
  plotBCV(d1)
  dev.off()
  
  et12 <- exactTest(d1) 
  
  png("MA.png")
  de1 <- decideTestsDGE(et12, adjust.method="BH", p.value=0.05)
  de1tags12 <- rownames(d1)[as.logical(de1)] 
  plotSmear(et12, de.tags=de1tags12)
  dev.off()
  
  nrDEG=topTags(et12, n=nrow(exprSet))
  nrDEG=as.data.frame(nrDEG)
  #write.csv(nrDEG,"edger_classic.results.csv",quote = F)
  
}