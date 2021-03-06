library(magrittr)
library(methods)
library(CancerSubtypes)

library("RTCGA.mRNA")

#  http://bioconductor.org/packages/3.6/bioc/vignettes/CancerSubtypes/inst/doc/CancerSubtypes-vignette.html
## prectice 


# data process ------------------------------------------------------------
## check the distribution of this data
### Prepare a TCGA gene expression dataset for analysis. 
data(BRCA.mRNA)
mRNA=t(as.matrix(BRCA.mRNA[,-1]))
colnames(mRNA)=BRCA.mRNA[,1]
###To observe the mean, variance and Median Absolute Deviation distribution of the dataset, it helps users to get the distribution characteristics of the data, e.g. To evaluate whether the dataset fits a normal distribution or not.
data.checkDistribution(mRNA)

## dealing with missing value(NA)
index=which(is.na(mRNA))
res1=data.imputation(mRNA,fun="median")
res2=data.imputation(mRNA,fun="mean")
res3=data.imputation(mRNA,fun="microarray")

## data normalization
result1=data.normalization(mRNA,type="feature_Median",log2=FALSE)
result2=data.normalization(mRNA,type="feature_zscore",log2=FALSE)

# # 2.2 Feature selection
# 2.2.1 Feature selection based on the most variance.
###The top 1000 most variance features will be selected.
data1=FSbyVar(mRNA, cut.type="topk",value=1000)

###The features with (variance>0.5) are selected.
data2=FSbyVar(mRNA, cut.type="cutoff",value=0.5)

# 2.2.2 Feature selection based on the most variant Median Absolute Deviation (MAD).
data1=FSbyMAD(mRNA, cut.type="topk",value=1000)
data2=FSbyMAD(mRNA, cut.type="cutoff",value=0.5)

# 2.2.3 Feature dimension reduction and extraction based on Principal Component Analysis.
mRNA1=data.imputation(mRNA,fun="microarray")
data1=FSbyPCA(mRNA1, PC_percent=0.9,scale = TRUE)

# 2.2.4 Feature selection based on Cox regression model.
data(GeneExp)
data(time)
data(status)
data1=FSbyCox(GeneExp,time,status,cutoff=0.05)
# clustering methods--------------------------------------------------------------

## 1. Consensus Clustering for cancer subtype identification ------------------
### Consensus clustering (CC, 2003) as an unsupervised subtypes discovery method, was a frequently used and valuable approach in many genomic studies and have lots of successful applications.
### The input dataset is single gene expression matrix.
data(GeneExp)
result1.1=ExecuteCC(clusterNum=3,d=GeneExp,maxK=10,clusterAlg="hc",distance="pearson",title="GBM")

### The input dataset is multi-genomics data as a list
data(GeneExp)
data(miRNAExp)
GBM=list(GeneExp=GeneExp,miRNAExp=miRNAExp)
result1.2=ExecuteCC(clusterNum=3,d=GBM,maxK=10,clusterAlg="hc",distance="pearson",title="GBM")


## 2.  Consensus Non-negative matrix factorization for cancer subty --------
### Non-negative matrix factorization (CNMF, 2004), as an effective dimension reduction method, was used in distinguishing molecular patterns for high-dimensional genomic data and provided a powerful method for class discovery. We apply the NMF package to execute the non-negative matrix factorization for the cancer genomic dataset. So this method allows users to input the number of core-CPUs for parallel processing.
### The input dataset is single gene expression matrix.
data(GeneExp)
result2.1=ExecuteCNMF(GeneExp,clusterNum=3,nrun=30)

### The input dataset is multi-genomics data as a list
data(GeneExp)
data(miRNAExp)
GBM=list(GeneExp=GeneExp,miRNAExp=miRNAExp)
result2.2=ExecuteCNMF(GBM,clusterNum=3,nrun=30)


## 3. Integrative clustering for cancer subtype identification -------------
### Integrative clustering (iCluster, 2009) used a joint latent variable model for integrative clustering for multiple types of omics data.
data(GeneExp)
data(miRNAExp)
data1=FSbyVar(GeneExp, cut.type="topk",value=1000) # why feature selection here?
data2=FSbyVar(miRNAExp, cut.type="topk",value=230) # why feature selection here?
GBM=list(GeneExp=data1,miRNAExp=data2)
result3.1=ExecuteiCluster(datasets=GBM, k=3, lambda=list(0.44,0.33,0.28))

## 4 Similarity network fusion for cancer subtype identification
### Similarity network fusion (SNF, 2014) is a computational method on fusion similarity network for aggregating multi-omics data.
data(GeneExp)
data(miRNAExp)
GBM=list(GeneExp=GeneExp,miRNAExp=miRNAExp)
result4=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20)

## 4. Ensemble method of SNF and CC for cancer subtype identification
### We propose to combine the SNF and CC together to generate a new cancer subtypes identification method.

data(GeneExp)
data(miRNAExp)
data(time)
data(status)
data1=FSbyCox(GeneExp,time,status,cutoff=0.05)
data2=FSbyCox(miRNAExp,time,status,cutoff=0.05)
GBM=list(GeneExp=data1,miRNAExp=data2)
result=ExecuteSNF.CC(GBM, clusterNum=3, K=20, alpha=0.5, t=20,maxK = 10, pItem = 0.8,reps=500, 
                     title = "GBM", plot = "png", finalLinkage ="average") # whta the meaning of result pic 11?12?13?

## 5 Weighted Similarity network fusion for cancer subtype identification
### WSNF is a caner subtype identificaton method with the assistance of the gene regulatory network information. It makes use of the miRNA-TF-mRNA regulatory network to take the importance of the features into consideration.
#### We can have a try when doing network analysis.

data(GeneExp)
data(miRNAExp)
data(Ranking)
####Retrieve there feature ranking for genes
gene_Name=rownames(GeneExp)
index1=match(gene_Name,Ranking$mRNA_TF_miRNA.v21_SYMBOL)
gene_ranking=data.frame(gene_Name,Ranking[index1,],stringsAsFactors=FALSE)
index2=which(is.na(gene_ranking$ranking_default))
gene_ranking$ranking_default[index2]=min(gene_ranking$ranking_default,na.rm =TRUE)

####Retrieve there feature ranking for genes
miRNA_ID=rownames(miRNAExp)
index3=match(miRNA_ID,Ranking$mRNA_TF_miRNA_ID)
miRNA_ranking=data.frame(miRNA_ID,Ranking[index3,],stringsAsFactors=FALSE)
index4=which(is.na(miRNA_ranking$ranking_default))
miRNA_ranking$ranking_default[index4]=min(miRNA_ranking$ranking_default,na.rm =TRUE)
###Clustering
ranking1=list(gene_ranking$ranking_default ,miRNA_ranking$ranking_default)
GBM=list(GeneExp,miRNAExp)
result=ExecuteWSNF(datasets=GBM, feature_ranking=ranking1, beta = 0.8, clusterNum=3, 
                   K = 20,alpha = 0.5, t = 20, plot = TRUE)
#### not really understand


# Results validation, interpretation and visualization for the identified cancer subtypes --------
## The identified cancer subtypes by the computational methods should be in accordance with biological meanings and reveal the distinct molecular patterns.


## 1. Silhouette width -----------------------------------------------------
### Silhouette width is used to measure how similar a sample is matched to its identified subtype compared to other subtypes, a high value indicates that the sample is well matched. Each horizontal line represents a sample in the Silhouette plot. The length of the line is the silhouette width the sample has.

###Similarity smaple matrix
sil=silhouette_SimilarityMatrix(result4$group, result4$distanceMatrix)
plot(sil)

### Note: If the input matrix is a dissimilarity matrix between samples, please use the silhouette() in cluster package to compute the silhouette width, otherwise a wrong result will be generated.
# All the samples have the negative silhouette width.
sil1=silhouette(result$group, result$distanceMatrix)
plot(sil1)  ##wrong result


## 2. survival analysis -----------------------------------------------------
### Survival analysis is used to judge the different survival patterns between subtypes.

data(GeneExp)
data(miRNAExp)
data(time)
data(status)
data1=FSbyCox(GeneExp,time,status,cutoff=0.05)
data2=FSbyCox(miRNAExp,time,status,cutoff=0.05)
GBM=list(GeneExp=data1,miRNAExp=data2)

#### 2.1.ExecuteSNF
result1=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20,plot = FALSE)
group1=result1$group
distanceMatrix1=result1$distanceMatrix
p_value=survAnalysis(mainTitle="GBM1",time,status,group1,
                     distanceMatrix1,similarity=TRUE)
### This is a combination figure with three parts: Survival curves, heatmap of the sample similarity matrix and Silhouette width plots for the identified cancer subtypes.

#### 2.2.ExecuteSNF.CC
result2=ExecuteSNF.CC(GBM, clusterNum=3, K=20, alpha=0.5, t=20,
                      maxK = 5, pItem = 0.8,reps=500, 
                      title = "GBM2", plot = "png",
                      finalLinkage ="average")
group2=result2$group
distanceMatrix2=result2$distanceMatrix
p_value=survAnalysis(mainTitle="GBM2",time,status,group2,
                     distanceMatrix2,similarity=TRUE)


#### 3 Statistical significance of clustering---------
## Statistical significance of clustering is a pure statistical approach to test the significance difference data distribution between subtypes. Different expression is to test the expression difference between each subtypes and a reference group (always a set of normal samples).

data(GeneExp)
data(miRNAExp)
data(time)
data(status)
GBM=list(GeneExp=GeneExp,miRNAExp=miRNAExp)
result=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20,plot = FALSE)
group=result$group
sigclust=sigclustTest(miRNAExp,group, nsim=1000, nrep=1, icovest=1) #A statistical method for testing the significance of clustering results.
sigclust 


### 4. Differential expression analysis -------------------
###Differential expression analysis is to test the expression difference between each subtypes and a reference group (always a set of normal samples). Here we apply limma package to conduct the different expression analysis between each subtypes and normal samples.

library("RTCGA.mRNA")
#require(TCGAbiolinks)
rm(list = ls())
data(BRCA.mRNA)
mRNA=t(as.matrix(BRCA.mRNA[,-1]))
colnames(mRNA)=BRCA.mRNA[,1]
mRNA1=data.imputation(mRNA,fun="microarray")
mRNA1=FSbyMAD(mRNA1, cut.type="topk",value=5000)
###Split the normal and tumor samples
index=which(as.numeric(substr(colnames(mRNA1),14,15))>9)
mRNA_normal=mRNA1[,index]
mRNA_tumor=mRNA1[,-index]

### Remove the duplicate samples
index1=which(as.numeric(substr(colnames(mRNA_tumor),14,15))>1)
mRNA_tumor=mRNA_tumor[,-index1]

##### Identify cancer subtypes
result=ExecuteCC(clusterNum=5,d=mRNA_tumor,maxK=5,clusterAlg="hc",distance="pearson",title="BRCA")
group=result$group
res=DiffExp.limma(Tumor_Data=mRNA_tumor,Normal_Data=mRNA_normal,group=group,topk=NULL,RNAseq=FALSE)
## Differently expression genes in subtype 1
head(res[[1]])