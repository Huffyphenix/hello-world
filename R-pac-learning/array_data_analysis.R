
#  package preparation ----------------------------------------------------

source("http://bioconductor.org/biocLite.R")

biocLite("GEOquery")

library(GEOquery)

setwd("F:/ovrian_depression")

# data
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE9116

eSet <- getGEO('GSE9116', destdir = '.', getGPL = F, AnnotGPL = F)#һ��оƬ��ע���ļ��Ƚϴ�ֱ�����ز����׳ɹ���������ѡ����Ա�������GPL��Annotation
save(eSet, file = 'GSE62832.eSet.Rdata')
load("GSE62832.eSet.Rdata")
class(eSet) #�Ȳ鿴�������ࡾ�б��������ݿ�
str(eSet) #�ٿ����ݽṹ�����Կ����ڶ����漰���˱������
#��Ȼ���б�����ô���б�����ȡ��Ϣ���� eSet[[1]]
exp <- exprs(eSet[[1]])

# annotation platform: GPL96
# https://www.jianshu.com/p/f6906ba703a0
# 27 	GPL96 	Homo sapiens 	hgu133a
biocLite("hgu133a.db")
library("hgu133a.db")
ls("package:hgu133a.db")
symbol <- toTable(hgu133aSYMBOL) #��SYMBOL����תΪ���ݿ�

library(magrittr) 
symbol$symbol %>% unique() %>% length() #��һ���ж��ٻ��������������ױ�������2w���ҡ�
symbol$symbol %>% table() %>% sort() %>% tail() #����ʹ��̽������ǰ6����������������������10��̽�롿
symbol$symbol %>% table() %>% sort() %>% table()


dim(exp) #����ǰ
#���ˡ�ֻ������ע���ļ�̽��id��ͬ��̽�롿
efilt <- exp[rownames(exp)%in%symbol$probe_id,]
dim(efilt)#���˺�
#����1��Ŀ�ģ���֤һ�������Ӧһ��̽�룻��������̽��һһ��Ӧ�ܺ�˵�������һ�������Ӧ���̽�룺ÿ��̽��ȡһ�еľ�ֵ-����Ӧͬһ�����̽��ȡ����������̽��-�����ջ����������ǽ���������Ϊ�ǰ��ջ���������̽�루����s$probe_id����������ԭ���ǣ���������ǵ�Ŀ������ע�Ͱ���һ�������Ӧ�����Լ���������һ��̽�롣�����s$probe_id��ô����ͳ�����ע�Ͱ���һ��̽���Ӧ�����Լ���������һ��̽�룬��Ȼ����Ҳ���в��ɹ�����Ϊ�Լ���������̽����˺��������ע�Ͱ���̽����������ȣ�����û��һһ��Ӧ���������������ǲ���ģ�ʲô���������Բ���Ӧ���ľ�����������
maxp = by(efilt,symbol$symbol,function(x) rownames(x)[which.max(rowMeans(x))]) 
uniprobes = as.character(maxp)
efilt=efilt[rownames(efilt)%in%uniprobes,]
#����2��Ŀ�ģ������Ǳ��������������ɸղ�һ��һ�Ļ�����������match���������֤�˱�������ע�Ͱ���˳����һ�µġ�
rownames(efilt)=symbol[match(rownames(efilt),symbol$probe_id),2]


# 3.1 ���һЩ�ܼһ��������
boxplot(efilt[,1])#������һ���������������������ֲ������Կ�������Ϊ5����
efilt['ACTB',] #��������Beta-actin�Ļ�������ACTB���ܼһ���
efilt['GAPDH',] #Ҳ�ǹܼһ���

# 3.2 ��������������ֲ�

# �Ȱѱ������=��tidy data�����У��������������������������ͷ���(LowDepression and HighDepression)��

library(reshape2)
pdata=pData(eSet[[1]]) #������������Ϣ�����ݿ�����ȡ������ȡ�������Ǳ��͡����������ݿ�
group_list=as.character(pdata$characteristics_ch1) 
m_efilt = melt(efilt) #�Ƚ�ԭ�������ڻ�
colnames(m_efilt)=c('symbol','sample','value') #������������
m_efilt$group=rep(group_list,each=nrow(efilt))

# �Ա��������в������
suppressMessages(library(limma))
#limma��Ҫ�������󣺱������efilt�����������design�����ȽϾ���contrast��
#����һ���������design��˵��MAO���ļ���������MNO�����ļ���������1�������ǡ�
design <- model.matrix(~0+factor(group_list))
colnames(design) <- levels(factor(group_list))
rownames(design) <- colnames(efilt)
design
#����һ���ȽϾ���һ����case��control��
contrast<-makeContrasts(paste0(c("HighDepression","LowDepression"),collapse = "-"),levels = design)
contrast

DEG <- function(efilt,design,contrast){
  ##step1
  fit <- lmFit(efilt,design)
  ##step2
  fit2 <- contrasts.fit(fit, contrast) 
  fit2 <- eBayes(fit2)  
  ##step3
  mtx = topTable(fit2, coef=1, n=Inf)
  deg_mtx = na.omit(mtx) 
  return(deg_mtx)
}
DEG_mtx <- DEG(efilt,design,contrast) #�õ�ȫ���Ĳ���������


# The volcano figure
library(ggplot2)
DEG_mtx %>%
  dplyr::mutate(`-log10Pvalue` = -log10(P.Value)) %>%
  dplyr::mutate(group = ifelse(P.Value<=0.05&logFC>=0.585, "Up", "Not")) %>%
  dplyr::mutate(group = ifelse(P.Value<=0.05&logFC<=-0.585, "Down", group)) %>%
  ggplot(aes(x=logFC,y=`-log10Pvalue`)) +
  geom_point(aes(color=group)) +
  scale_color_manual(values = c("#4876FF", "#0A0A0A","#FF6A6A" ))

ggsave("DEG_volcano_plot.tiff",device = "tiff", height = 3,width = 4)


# enrichment analysis
DEG_mtx %>%
  dplyr::mutate(`-log10Pvalue` = -log10(P.Value)) %>%
  dplyr::mutate(DE_group = ifelse(P.Value<=0.05&logFC>0.585, "Up", "Not")) %>%
  dplyr::mutate(DE_group = ifelse(P.Value<=0.05&logFC<-0.585, "Down", DE_group)) 

library(clusterProfiler)

entrez <- toTable(hgu133aENTREZID) #��SYMBOL����תΪ���ݿ�
entrez %>%
  dplyr::inner_join(symbol,by="probe_id") %>%
  dplyr::select(gene_id,symbol) %>%
  unique() -> symbol_entrez
DEG_mtx %>%
  dplyr::mutate(`-log10Pvalue` = -log10(P.Value)) %>%
  dplyr::mutate(DE_group = ifelse(P.Value<=0.05 & logFC>0.585, "Up", "Not")) %>%
  dplyr::mutate(DE_group = ifelse(P.Value<=0.05 & logFC<(-0.585), "Down", DE_group))%>%
  dplyr::mutate(symbol=rownames(DEG_mtx)) %>%
  dplyr::inner_join(symbol_entrez,by="symbol") -> DEG_mtx.entrez

DEG_mtx.entrez %>%
  readr::write_tsv("DEG_result.tsv")


# go for upregulated genes
DEG_mtx.entrez %>%
  dplyr::filter(DE_group == "Up") -> up.mtx.entrez
go <- enrichGO(up.mtx.entrez$gene_id, OrgDb = "org.Hs.eg.db", ont="all", readable=TRUE)
go %>%
  as.data.frame() %>%
  dplyr::as.tbl() %>%
  dplyr::mutate(term = paste(ID,Description,sep = "~")) -> go_up_info
go_up_info %>%
  ggplot(aes(x=term,y=Count)) +
  geom_col(aes(fill = ONTOLOGY)) +
  coord_flip() +
  facet_grid(ONTOLOGY~.,scales = "free",space = "free") +
  theme(
    legend.position = "none",
    axis.text = element_text(colour = "black"),
    strip.text = element_text(colour = "black",size=12),
    plot.background = element_rect(fill = "white", colour = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
ggsave("UP_GO_enrichment.tiff",device = "tiff",width = 8,height = 6)
go_up_info %>% readr::write_tsv("UP_GO_enrichment_result.tsv")

# go for upregulated genes
DEG_mtx.entrez %>%
  dplyr::filter(DE_group == "Down") -> down.mtx.entrez
go <- enrichGO(down.mtx.entrez$gene_id, OrgDb = "org.Hs.eg.db", ont="all", readable=TRUE)
go %>%
  as.data.frame() %>%
  dplyr::as.tbl() %>%
  dplyr::mutate(term = paste(ID,Description,sep = "~")) -> go_down_info
go_down_info %>%
  ggplot(aes(x=term,y=Count)) +
  geom_col(aes(fill = ONTOLOGY)) +
  coord_flip() +
  facet_grid(ONTOLOGY~.,scales = "free",space = "free") +
  theme(
    legend.position = "none",
    axis.text = element_text(colour = "black"),
    strip.text = element_text(colour = "black",size=12),
    plot.background = element_rect(fill = "white", colour = "black"),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  ggsave("DOWN_GO_enrichment.tiff",device = "tiff",width = 10,height = 6)
go_down_info %>% readr::write_tsv("Down_GO_enrichment_result.tsv")
  
# kegg enrichmeny
DEG_mtx.entrez %>%
  dplyr::filter(DE_group != "Not") -> DEG_mtx.entrez.DE
kk <- enrichKEGG(gene         = DEG_mtx.entrez.DE$gene_id,
                 organism     = 'hsa',
                 pvalueCutoff = 1)
kk %>% 
  as.data.frame() -> kegg.DEG 

kegg.DEG %>%
  ggplot(aes(x=Description,y=Count)) +
  geom_col() +
  coord_flip() +
  theme(
    legend.position = "none",
    axis.text = element_text(colour = "black"),
    strip.text = element_text(colour = "black",size=12),
    plot.background = element_rect(fill = "white", colour = "black"),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  xlab("KEGG pathways")
ggsave("KEGG_enrichment.tiff",device = "tiff",width = 4,height = 3)

kegg.DEG %>%
  tidyr::separate(geneID,paste("gene",1:8,sep="_"),"/") %>%
  dplyr::select(-ID,-GeneRatio,-BgRatio,-pvalue,-Count) %>%
  tidyr::gather(-Description,-p.adjust,-qvalue,key="title",value="gene_id") %>%
  tidyr::drop_na() %>%
  dplyr::inner_join(symbol_entrez,by="gene_id") %>%
  dplyr::select(Description,title,symbol) %>%
  tidyr::spread(key = title,value=symbol) %>%
  tidyr::unite("enriched_genes",paste("gene",1:8,sep="_") %>% unique(),sep="/") %>%
  dplyr::mutate(enriched_genes=gsub("/NA","",enriched_genes)) %>%
  dplyr::inner_join(kegg.DEG,by="Description") %>%
  dplyr::select(-geneID) %>%
  readr::write_tsv("KEGG_enrichment_result.tsv")