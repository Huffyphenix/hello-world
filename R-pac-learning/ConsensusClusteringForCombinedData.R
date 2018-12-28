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
  dplyr::mutate(color=ifelse(PFS==1,"red","blue"))
mutation_info <- mutation_burden_class %>%
  dplyr::right_join(survival_info,by="barcode") %>%
  dplyr::select(barcode,mutation_status) %>%
  dplyr::mutate(color = ifelse(mutation_status=="NA","white","grey")) %>%
  dplyr::mutate(color = ifelse(mutation_status=="high_muation_burden","pink",color)) %>%
  dplyr::mutate(color = ifelse(is.na(color),"white",color))

cancer_color <- readr::read_tsv(file.path("/data/shiny-data/GSCALite","02_pcc.tsv"))
cancer_info <- cnv_merge_snv_data %>%
  dplyr::select(cancer_types,barcode) %>%
  unique() %>%
  dplyr::filter(barcode %in% colnames(W)) %>%
  dplyr::inner_join(cancer_color,by = "cancer_types")
M_label=cbind(group,survival_info$PFS,mutation_info$mutation_status,cancer_info$cancer_types)
colnames(M_label)=c("spectralClustering","PFS.status","mutation_burden_class","cancer_types")
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
                     "cancer_types"=cancer_info$color)
## Visualize the clusters present in the given similarity matrix
## as well as some sample information
## In this presentation no clustering method is ran the samples
## are ordered in function of their group label present in the group arguments
displayClustersWithHeatmap(W, group, M_label_colors[,"spectralClustering"],col = c("blue","red"))
displayClustersWithHeatmap(W, group, M_label_colors)
