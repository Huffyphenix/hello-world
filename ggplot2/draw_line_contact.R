##############################################
# to draw a line contact plot use ggplot2
##############################################

# test data ---------------------------------------------------------------
config<-list()
config$database<-"/data/GSCALite/TCGA/rppa" 
gene_rppa_pval <- readr::read_rds(file.path(config$database,"pan32_gene_A-I-N_percent.rds.gz"))

gene_rppa_pval %>%
  tidyr::unnest() %>%
  tidyr::drop_na() %>%
  dplyr::filter(symbol %in% c("PRAMEF10","ZZZ3")) %>%
  # dplyr::filter(fdr <= 0.05) %>%
  dplyr::mutate(pathway = plyr::revalue(pathway, pathway_replace)) %>%
  dplyr::mutate(class = ifelse(fdr <= 0.05 & diff > 0, "Activation", "None")) %>%
  dplyr::mutate(class = ifelse(fdr <= 0.05 & diff < 0, "Inhibition", class)) %>%
  dplyr::filter(class!="None") -> gene_rppa_sig_pval_class

# get cancer text in plot -------------------------------------------------
get_text <- function(gene_rppa_sig_pval_class) {
  c.text <- data.frame(x = 1, y = 1, text = "test", type = "test")

  gene_rppa_sig_pval_class %>%
    dplyr::pull(cancer_types) %>%
    unique() -> cancer.text
  for (i in 1:length(cancer.text)) {
    data.frame(x = 1, y = i, text = cancer.text[i], type = "cancer") -> tmp.text
    rbind(c.text, tmp.text) -> c.text
  }

  gene_rppa_sig_pval_class %>%
    dplyr::pull(symbol) %>%
    unique() -> gene.text
  for (i in 1:length(gene.text)) {
    data.frame(x = 3, y = i, text = gene.text[i], type = "gene") -> tmp.text
    rbind(c.text, tmp.text) -> c.text
  }

  gene_rppa_sig_pval_class %>%
    dplyr::pull(pathway) %>%
    unique() -> pathway.text
  for (i in 1:length(pathway.text)) {
    data.frame(x = 5, y = i, text = pathway.text[i], type = "pathway") -> tmp.text
    rbind(c.text, tmp.text) -> c.text
  }
  return(c.text[-1,])
}

# get segment for geom_seq ------------------------------------------------

get_seg <- function(cancer_text,gene_rppa_sig_pval_class){
  .d_seg <- data.frame(x1=0,y1=0,x2=0,y2=0,Cancer="test",Regulation="test")
  nrow(gene_rppa_sig_pval_class) ->n
  for(i in 1:n){
    gene_rppa_sig_pval_class[i,1] ->cancer
    gene_rppa_sig_pval_class[i,2] ->gene
    gene_rppa_sig_pval_class[i,3] ->pathway
    gene_rppa_sig_pval_class[i,6] ->diff
    if(diff>0){line_type="Activate"}else{line_type="Inhibit"}
    cancer_text %>%
      dplyr::filter(text %in% cancer) %>%
      dplyr::select(x,y) ->c.pos
    cancer_text %>%
      dplyr::filter(text %in% gene) %>%
      dplyr::select(x,y) ->g.pos
    cancer_text %>%
      dplyr::filter(text %in% pathway) %>%
      dplyr::select(x,y) ->p.pos
    .d_seq_tmp1<- data.frame(x1=c.pos$x,y1=c.pos$y,x2=g.pos$x,y2=g.pos$y,Cancer=cancer$cancer_types,Regulation="Activate")
    .d_seq_tmp2<- data.frame(x1=g.pos$x,y1=g.pos$y,x2=p.pos$x,y2=p.pos$y,Cancer=cancer$cancer_types,Regulation=line_type)
    rbind(.d_seg,.d_seq_tmp1) %>% rbind(.d_seq_tmp2) -> .d_seg
  }
  .d_seg[-1,] %>%
    unique() ->.d_seg
  return(.d_seg)
}

# get data ----------------------------------------------------------------

cancer_text <- get_text(gene_rppa_sig_pval_class)
plot_seg <- get_seg(cancer_text,gene_rppa_sig_pval_class) 

cancer_text %>%
  dplyr::filter(type=="cancer") ->cancer.text
cancer_text %>%
  dplyr::filter(type=="gene") ->gene.text
cancer_text %>%
  dplyr::filter(type=="pathway") ->path.text


# draw plot ---------------------------------------------------------------

library(ggplot2)
ggplot() +
  geom_segment(data = plot_seg, mapping = aes(
    x = x1, 
    y = y1,
    xend = x2,
    yend = y2,
    colour = Cancer,
    linetype = Regulation
  )) +
  guides(color=FALSE) +
  geom_text(
    data = cancer.text, 
    mapping = aes(x = x, y = y, label = text,color=text),
    hjust = 1,
    ) +
  geom_text(
    data = gene.text, 
    mapping = aes(x = x, y = y-0.15, label = text)) +
  geom_text(
    data = path.text, 
    mapping = aes(x = x, y = y, label = text),
    hjust=0) +
  expand_limits(x=c(0,7)) +
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
