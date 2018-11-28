
# gene list ---------------------------------------------------------------

genelist <- c("CBX2","CBX4","CBX6","CBX7","CBX8") 


# load exp ----------------------------------------------------------------

exp <-readr::read_tsv("E:/data/TCGA/lung_DE/CBX24678/all_genes_exp.confirm")


# filter ------------------------------------------------------------------

library(magrittr)
exp %>%
  dplyr::filter(gene_id %in% genelist) %>%
  tidyr::gather(-gene_id,key="sample",value="Expression") %>%
  # .[-1,] %>%
  # dplyr::rename("PRDM5_RSEM"="value") %>%
  dplyr::mutate(Group=substr(sample,6,7)) %>%
  dplyr::mutate(Group=ifelse(Group=="01","Tumor","Normal")) -> genelist_exp
  # dplyr::select(-key) %>%
  # dplyr::mutate(PRDM5_RSEM=as.numeric(PRDM5_RSEM))->PRDM5

# pvalue calculation ------------------------------------------------------
genelist_exp %>%
  dplyr::group_by(gene_id) %>%
  dplyr::do(broom::tidy(oneway.test(Expression ~Group,data=.))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(fdr = p.adjust(p.value, method = "fdr")) %>%
  dplyr::select(gene_id,fdr) %>%
  dplyr::mutate(label=paste0("FDR = ",signif(fdr, 3)))-> genelist_stage_pvalue


genelist_exp %>%
  dplyr::filter(Group=="Tumor") %>%
  dplyr::group_by(gene_id) %>%
  dplyr::mutate(t_e=sum(Expression)) %>%
  dplyr::select(gene_id,t_e) %>%
  dplyr::ungroup() %>%
  unique()-> genelist_tumor_exp

genelist_exp %>%
  dplyr::filter(Group=="Normal") %>%
  dplyr::group_by(gene_id) %>%
  dplyr::mutate(n_e=sum(Expression)) %>%
  dplyr::select(gene_id,n_e) %>%
  dplyr::ungroup() %>%
  unique() %>%
  dplyr::inner_join(genelist_tumor_exp,by="gene_id") %>%
  dplyr::mutate(`log2(FC)`=paste0("Log2FC = ",round(log2(t_e/n_e),2),sep="")) %>%
  dplyr::mutate(x=c(1:5)) %>%
  dplyr::mutate(y=3000) -> genelist_FC
  # dplyr::mutate(x=ifelse(gene_id=="CBX2",1,0)) %>%
  # dplyr::mutate(x=ifelse(gene_id=="CBX4",2,x)) %>%
  # dplyr::mutate(x=ifelse(gene_id=="CBX6",3,x)) %>%
  # dplyr::mutate(x=ifelse(gene_id=="CBX7",4,x)) %>%
  # dplyr::mutate(x=ifelse(gene_id=="CBX8",5,x)) 



library(ggplot2)
genelist_exp$gene_id %>%
  plyr::revalue(c(CBX2="CBX2 (Log2FC = 2.54)",
                          CBX4="CBX4 (Log2FC = 0.95)",
                          CBX6="CBX6 (Log2FC = -0.59)",
                          CBX7="CBX7 (Log2FC = -1.64)",
                          CBX8="CBX8 (Log2FC = 1.17)")) ->genelist_exp$gene_id
genelist_exp%>%
  ggplot2::ggplot(mapping=aes(x=Group,y=Expression,color=Group)) +
  geom_boxplot() +
  geom_point(aes(x=Group,y=Expression,color=Group)) +
  facet_grid(~gene_id) +
  theme(
    axis.line = element_line(color = "black"),
    panel.background  = element_rect(fill = "white", color = "grey"),
    panel.grid = element_line(colour = "grey"),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) -> p

ann_text <- data.frame(Group = "Tumor",Expression = 3000,lab = genelist_stage_pvalue$label,
                       gene_id = factor(genelist_stage_pvalue$gene_id,levels = genelist_stage_pvalue$gene_id))
p + geom_text(data = ann_text,aes(label=ann_text$lab))
  ggsave("E:/data/TCGA/lung_DE/CBX24678/DE_plot.pdf",device = "pdf",width = 14,height = 4)
  