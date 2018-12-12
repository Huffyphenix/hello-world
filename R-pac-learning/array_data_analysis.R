
#  package preparation ----------------------------------------------------

source("http://bioconductor.org/biocLite.R")

biocLite("GEOquery")

library(GEOquery)

setwd("F:/ovrian_depression")

# data
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE9116

eSet <- getGEO('GSE9116', destdir = '.', getGPL = F, AnnotGPL = F)#一般芯片的注释文件比较大，直接下载不容易成功，后两个选项可以避免下载GPL、Annotation
save(eSet, file = 'GSE62832.eSet.Rdata')
load("GSE62832.eSet.Rdata")
class(eSet) #先查看数据种类【列表还是数据框】
str(eSet) #再看数据结构【可以看到第二行涉及到了表达矩阵】
#既然是列表，那么从列表中提取信息就是 eSet[[1]]
exp <- exprs(eSet[[1]])

# annotation platform: GPL96
# https://www.jianshu.com/p/f6906ba703a0
# 27 	GPL96 	Homo sapiens 	hgu133a
biocLite("hgu133a.db")
library("hgu133a.db")
ls("package:hgu133a.db")
symbol <- toTable(hgu133aSYMBOL) #将SYMBOL对象转为数据框

library(magrittr) 
symbol$symbol %>% unique() %>% length() #看一下有多少基因【人类正常蛋白编码基因就2w左右】
symbol$symbol %>% table() %>% sort() %>% tail() #基因使用探针最多的前6名【发现有两个基因用了10个探针】
symbol$symbol %>% table() %>% sort() %>% table()


dim(exp) #过滤前
#过滤【只保留和注释文件探针id相同的探针】
efilt <- exp[rownames(exp)%in%symbol$probe_id,]
dim(efilt)#过滤后
#整合1【目的：保证一个基因对应一个探针；如果基因和探针一一对应很好说，但如果一个基因对应多个探针：每个探针取一行的均值-》对应同一基因的探针取表达量最大的探针-》按照基因名给他们建索引，因为是按照基因来过滤探针（不用s$probe_id构建索引的原因是，看清楚我们的目的是让注释包的一个基因对应我们自己表达矩阵的一个探针。如果用s$probe_id那么结果就成了让注释包的一个探针对应我们自己表达矩阵的一个探针，当然这样也运行不成功，因为自己表达矩阵的探针过滤后的数量和注释包的探针数量不相等，这样没法一一对应。但基因名数量是不变的，什么是索引？以不变应万变的就是索引）】
maxp = by(efilt,symbol$symbol,function(x) rownames(x)[which.max(rowMeans(x))]) 
uniprobes = as.character(maxp)
efilt=efilt[rownames(efilt)%in%uniprobes,]
#整合2【目的：将我们表达矩阵的行名换成刚才一对一的基因名，并且match这个函数保证了表达矩阵和注释包的顺序是一致的】
rownames(efilt)=symbol[match(rownames(efilt),symbol$probe_id),2]


# 3.1 检测一些管家基因表达量
boxplot(efilt[,1])#看看第一个样本中总体基因表达量分布，可以看到基本为5左右
efilt['ACTB',] #激动蛋白Beta-actin的基因名是ACTB，管家基因
efilt['GAPDH',] #也是管家基因

# 3.2 看表达矩阵的整体分布

# 先把表达矩阵=》tidy data【四列：基因名、样本、表达量、表型分组(LowDepression and HighDepression)】

library(reshape2)
pdata=pData(eSet[[1]]) #将样本表型信息从数据框中提取出来【取出来的是表型、样本的数据框】
group_list=as.character(pdata$characteristics_ch1) 
m_efilt = melt(efilt) #先将原来矩阵“融化
colnames(m_efilt)=c('symbol','sample','value') #重新命名三列
m_efilt$group=rep(group_list,each=nrow(efilt))

# 对表达矩阵进行差异分析
suppressMessages(library(limma))
#limma需要三个矩阵：表达矩阵（efilt）、分组矩阵（design）、比较矩阵（contrast）
#先做一个分组矩阵～design，说明MAO是哪几个样本，MNO又是哪几个，其中1代表“是”
design <- model.matrix(~0+factor(group_list))
colnames(design) <- levels(factor(group_list))
rownames(design) <- colnames(efilt)
design
#再做一个比较矩阵【一般是case比control】
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
DEG_mtx <- DEG(efilt,design,contrast) #得到全部的差异基因矩阵


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

entrez <- toTable(hgu133aENTREZID) #将SYMBOL对象转为数据框
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
