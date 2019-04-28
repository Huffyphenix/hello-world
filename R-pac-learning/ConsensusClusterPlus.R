source("https://bioconductor.org/biocLite.R")
biocLite("ConsensusClusterPlus")
library(ConsensusClusterPlus)
library(ALL)
data(ALL)
d=exprs(ALL)

# reduce the dataset to the top 5,000 most variable genes, measured by median absolute deviation(mad). 绝对中位差
mads=apply(d,1,mad)
d=d[rev(order(sort(mads)))[1:5000],] # 排序，获得rank值，倒置，取前5000

d = sweep(d,1, apply(d,1,median,na.rm=T)) ## median center genes

title=tempdir()

results = ConsensusClusterPlus(d,maxK=6,reps=50,pItem=0.8,pFeature=1,
                                title=title,clusterAlg="hc",distance="pearson",seed=1262118388.71279,plot="png")
results[[2]][['consensusClass']] #  sample distribution in two clusters

# same as above but with pre-computed distance matrix, useful for large datasets (>1,000's of items)
dt = as.dist(1-cor(dc,method="pearson"))
rcc2 = ConsensusClusterPlus(dt,maxK=4,reps=100,pItem=0.8,pFeature=1,title="example2",distance="pearson",clusterAlg="hc")
