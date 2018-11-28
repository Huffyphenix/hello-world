# stage for some genes, need by Chenghao Xuan -----
library(magrittr)
genelist <- c("CBX2","CBX4","CBX6","CBX7","CBX8")
# load data ---------------------------------------------------------------

data_path <-c("E:/data/TCGA/lung_stage")

stage1 <- read.table(file.path(data_path,"stage1.exp"), header = T)
stage1A <- read.table(file.path(data_path,"stage1A.exp"), header = T)
stage1B <- read.table(file.path(data_path,"stage1B.exp"), header = T)
stage2 <- read.table(file.path(data_path,"stage2and2A.exp"), header = T)
stage2B <- read.table(file.path(data_path,"stage2B.exp"), header = T)
stage3A <- read.table(file.path(data_path,"stage3A.exp"), header = T)
stage3B <- read.table(file.path(data_path,"stage3B.exp"), header = T)
stage4 <- read.table(file.path(data_path,"stage4.exp"), header = T)
library(magrittr)
stage1 %>%
  dplyr::inner_join(stage1A,by="gene_id") %>%
  dplyr::inner_join(stage1B,by="gene_id") -> stage1

stage2 %>%
  dplyr::inner_join(stage2B,by="gene_id") -> stage2

stage3A %>%
  dplyr::inner_join(stage3B,by="gene_id") -> stage3


stage1 %>%
  dplyr::filter(gene_id %in% genelist) %>%
  dplyr::as_tibble()-> stage1_genelist

stage2 %>%
  dplyr::filter(gene_id %in% genelist) %>%
  dplyr::as_tibble()-> stage2_genelist

stage3 %>%
  dplyr::filter(gene_id %in% genelist)%>%
  dplyr::as_tibble() -> stage3_genelist

stage4 %>%
  dplyr::filter(gene_id %in% genelist) %>%
  dplyr::as_tibble()-> stage4_genelist

stage1_genelist %>%
  tidyr::gather(-gene_id,key="sample",value="exp") %>%
  dplyr::mutate(Group="Stage1") -> stage1_genelist_gat

stage2_genelist %>%
  tidyr::gather(-gene_id,key="sample",value="exp") %>%
  dplyr::mutate(Group="Stage2") -> stage2_genelist_gat

stage3_genelist %>%
  tidyr::gather(-gene_id,key="sample",value="exp") %>%
  dplyr::mutate(Group="Stage3") -> stage3_genelist_gat

stage4_genelist %>%
  tidyr::gather(-gene_id,key="sample",value="exp") %>%
  dplyr::mutate(Group="Stage4") -> stage4_genelist_gat

stage1_genelist_gat %>%
  rbind(stage2_genelist_gat) %>%
  rbind(stage3_genelist_gat) %>%
  rbind(stage4_genelist_gat) %>%
  dplyr::rename(Expression="exp") %>%
  dplyr::mutate(Group=ifelse(Group=="Stage3","Stage3&4",Group)) %>%
  dplyr::mutate(Group=ifelse(Group=="Stage4","Stage3&4",Group)) -> stage_ready


# statstic ----------------------------------------------------------------
stage_ready %>%
  dplyr::group_by(gene_id) %>%
  dplyr::do(broom::tidy(oneway.test(Expression ~Group,data=.))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(fdr = p.adjust(p.value, method = "fdr")) %>%
  dplyr::select(gene_id,fdr) %>%
  dplyr::mutate(label=paste0("FDR = ",round(fdr,5)))-> genelist_stage_pvalue

library(ggplot2)
stage_ready %>%
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

ann_text <- data.frame(Group = "Stage2",Expression = 3500,lab = genelist_stage_pvalue$label,
                       gene_id = factor(genelist_stage_pvalue$gene_id,levels = genelist_stage_pvalue$gene_id))
p + geom_text(data = ann_text,aes(label=ann_text$lab))

ggsave(file.path(data_path,"CBX2-4-6-7-8_stage_plot.pdf"),device = "pdf",width = 12,height = 5)
