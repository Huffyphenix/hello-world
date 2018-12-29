# Consensus clustering for combined data -------
# TRY----
# load data ----
data_result_path <- "/project/huff/huff/immune_checkpoint/genelist_data"

load(file = file.path(data_result_path, ".rda_IMK_mutationburden_cancerSubtype_analysis.rda"))

gene_list_expr_with_mutation_load.matrix.combine[,1:100] -> expr_test
cnv_merge_snv_data.matrix.combine[,1:100] -> cnv_test
PanCan26_gene_list_methy_matrix.combine[,1:100] -> methy_test

index=which(is.na(expr_test)) # no NA value
expr_test=data.imputation(expr_test,fun="median")

index=which(is.na(cnv_test)) # no NA value
cnv_test=data.imputation(cnv_test,fun="median")

index=which(is.na(methy_test)) # no NA value
methy_test=data.imputation(methy_test,fun="median")


expr_test = sweep(expr_test,1, apply(expr_test,1,median,na.rm=T)) ## median center genes
cnv_test = sweep(cnv_test,1, apply(cnv_test,1,median,na.rm=T)) ## median center genes
methy_test = sweep(methy_test,1, apply(methy_test,1,median,na.rm=T)) ## median center genes

#expr_truelabel = c(matrix(1,9,1),matrix(2,60,1)); ## 可以是基因原来的分类，免疫功能the ground truth of the simulated data

# need do with SNF tools, please see SNFtool.R
library(SNFtool)
K = 20; # number of neighbors, usually (10~30)
alpha = 0.5; # hyperparameter, usually (0.3~0.8)
T = 20; # Number of Iterations, usually (10~20)
## Calculate the pair-wise distance;
## If the data is continuous, we recommend to use the function "dist2" as follows
expr_Dist = (SNFtool::dist2(t(as.matrix(expr_test)),t(as.matrix(expr_test))))^(1/2)
cnv_Dist = (SNFtool::dist2(t(as.matrix(cnv_test)),t(as.matrix(cnv_test))))^(1/2)
methy_Dist = (SNFtool::dist2(t(as.matrix(methy_test)),t(as.matrix(methy_test))))^(1/2)

## next, construct similarity graphs
expr_W = affinityMatrix(expr_Dist, K, alpha)
cnv_W = affinityMatrix(cnv_Dist, K, alpha)
methy_W = affinityMatrix(methy_Dist, K, alpha)

displayClusters(expr_W,expr_truelabel) # 

W = SNF(list(expr_W,cnv_W,methy_W), K, T)

C = 5 # determine the number of clusters

group = spectralClustering(W,C); # the final subtypes information
displayClusters(W, group) # rough heatmap 

## Get a matrix containing the group information
## for the samples such as the SpectralClustering result and the True label
survival_info <- time_status %>%
  dplyr::filter(barcode %in% colnames(W)) %>% # colnames(W) change to names(group) when doing single data set(expr, cnv and methy data)
  dplyr::mutate(color=ifelse(PFS==1,"red","blue")) %>%
  dplyr::mutate(PFS=ifelse(PFS==1,"Dead","Alive")) 

mutation_info <- mutation_burden_class %>%
  dplyr::right_join(survival_info,by="barcode") %>%
  dplyr::select(barcode,mutation_status) %>%
  dplyr::mutate(color = ifelse(mutation_status=="NA","white","grey")) %>%
  dplyr::mutate(color = ifelse(mutation_status=="high_muation_burden","pink",color)) %>%
  dplyr::mutate(color = ifelse(is.na(color),"white",color)) %>%
  dplyr::mutate(mutation_status = ifelse(is.na(mutation_status),"NA","High")) %>%
  dplyr::mutate(mutation_status = ifelse(color=="grey","Low",mutation_status))

purity_info <- tumor_purity %>%
  dplyr::right_join(survival_info,by="barcode") %>%
  dplyr::select(barcode,purity) %>%
  dplyr::mutate(purity = ifelse(is.na(purity),0,purity))
cancer_color <- readr::read_tsv(file.path("/data/shiny-data/GSCALite","02_pcc.tsv"))
cancer_info <- cnv_merge_snv_data %>%
  dplyr::select(cancer_types,barcode) %>%
  unique() %>%
  dplyr::filter(barcode %in% colnames(W)) %>%
  dplyr::inner_join(cancer_color,by = "cancer_types")
M_label=cbind(group,survival_info$PFS,mutation_info$mutation_status,cancer_info$cancer_types,purity_info$purity)
colnames(M_label)=c("spectralClustering","PFS.status","mutation_burden_class","cancer_types","TumorPurity")
## ****
## Comments
rownames(M_label)=colnames(W) # To add if the spectralClustering function
## pass the sample ID as names.
## or rownames(M_label)=rownames(W) Having W with rownames and colmanes
## with smaple ID would help as well.
## ***
## Use the getColorsForGroups function to assign a color to each group
## NB is more than 8 groups, you will have to input a vector
## of colors into the getColorsForGroups function
# M_label_colors=t(apply(M_label,1,getColorsForGroups))
## or choose you own colors for each label, for example:
cancer_info$cancer_types %>% unique() %>% length() -> cancer_n
M_label_colors=cbind("spectralClustering"=getColorsForGroups(M_label[,"spectralClustering"],
                                                             colors=rainbow(C)),
                     "PFS.status"=survival_info$color,
                     "mutation_burden_class"=mutation_info$color,
                     "cancer_types"=cancer_info$color,
                     "tumor_purity" = circlize::colorRamp2(c(0,
                                                             min(purity_info$purity[purity_info$purity>0]),
                                                             max(purity_info$purity[purity_info$purity>0])),
                                                           c("white","grey100","grey0"))(purity_info$purity)
)
## Visualize the clusters present in the given similarity matrix
## as well as some sample information
## In this presentation no clustering method is ran the samples
## are ordered in function of their group label present in the group arguments
# heatmap of the cluster 
pdf()
displayClustersWithHeatmap(W, group, M_label_colors[,"spectralClustering"],col = c("blue","red"))
displayClustersWithHeatmap(W, group, M_label_colors,col = RColorBrewer::brewer.pal(9,"YlOrRd"))
dev.off()

##############
# add legend, with another plot, need to ps them togather with AI.
pdf()
par(mfrow=c(1,1))
plot.new()
for (i in 1:ncol(M_label)) {
  legend(i*0.2,0.5,
         legend=c(M_label[,i] %>% unique()),
         fill=c(M_label_colors[,i] %>% unique()), 
         border=T, bty="n", y.intersp = 0.7, cex=0.7,
         title = colnames(M_label)[i])
}
dev.off()

##############
# make heatmap by ComplexHeatmap
library(ComplexHeatmap)

normalize <- function(X) X/rowSums(X)
ind <- sort(as.vector(group), index.return = TRUE)
ind <- ind$ix

(W) <- median(as.vector(W))
W <- normalize(W)
W <- W + t(W)

# color file prepare for complexheatmap
M_label=data.frame("spectralClustering"=group,"PFS.status"=survival_info$PFS,"mutation_burden_class"=mutation_info$mutation_status,"cancer_types"=cancer_info$cancer_types,"TumorPurity"=as.numeric(purity_info$purity))
rownames(M_label)=colnames(W) # To add if the spectralClustering function

M_label_colors[,"cancer_types"] %>% unique() -> cancer_anno
names(cancer_anno)= c(M_label$cancer_types %>% as.character() %>% unique())

M_label_colors[,"mutation_burden_class"] %>% unique() -> mutaion_anno
names(mutaion_anno)= c(M_label$mutation_burden_class %>% as.character() %>% unique())

M_label_colors[,"PFS.status"] %>% unique() -> survival_anno
names(survival_anno)= c(M_label$PFS.status %>% as.character() %>%unique())

M_label_colors[,"spectralClustering"] %>% unique() -> cluster_anno
names(cluster_anno)= c(M_label$spectralClustering %>% unique())

# M_label_colors[,"TumorPurity"] %>% unique() -> TumorPurity_anno
# circlize::colorRamp2(c(0,
#                        min(purity_info$purity[purity_info$purity>0]),
#                        max(purity_info$purity[purity_info$purity>0])),
#                      c("white","grey100","grey0"))(purity_info$purity) -> purity_anno
# names(purity_anno) = purity_info$purity
# unique(purity_anno) -> purity_anno
# 
# M_label[,"TumorPurity"]=as.numeric(M_label[,"TumorPurity"])
col_anno <- HeatmapAnnotation(df=M_label,
                          col = list("spectralClustering"=cluster_anno,
                                     "PFS.status"=survival_anno,
                                     "mutation_burden_class"=mutaion_anno,
                                     "cancer_types"=cancer_anno,
                                     "TumorPurity"=circlize::colorRamp2(c(0,
                                                                          min(M_label$TumorPurity[M_label$TumorPurity>0]),
                                                                          max(M_label$TumorPurity[M_label$TumorPurity>0])),
                                                                        c("white","grey100","grey0"))),
                           
                           width = unit(0.5, "cm"))
draw(col_anno,1:20)


library(circlize)
he = Heatmap(W[ind, ind],
             col = RColorBrewer::brewer.pal(9,"YlOrRd"),
             show_row_names = FALSE, 
             show_column_names = FALSE,
             cluster_columns = FALSE,
             cluster_rows = FALSE,
             show_row_dend = FALSE, # whether show row clusters.
             top_annotation = col_anno,show_heatmap_legend = F,
             # heatmap_legend_param = list(title = c("Scaled Exp."))
             )
pdf()
he
dev.off()