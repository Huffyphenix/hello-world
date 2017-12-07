## First, let's load the libraries and read in the data files. A sample annotation file was included with the package:
library(MIRA)
library(data.table) # for the functions: fread, setkey, merge
library(GenomicRanges) # for the functions: GRanges, resize
library(ggplot2) # for the function: ylab

# get data from system file, fread is a fast read function
exampleAnnoDT2 <- fread(system.file("extdata", "exampleAnnoDT2.txt",
                                    package="MIRA")) 

## Next, construct file names for the DNA methylation samples and the region sets 
## (downloaded through the link in the Input section).
# 12 Ewing samples: T1-T12
pathToData <- "/path/to/MIRA_Ewing_Vignette_Files/"
ewingFiles <- paste0(pathToData, "EWS_T", 1:12, ".bed")
# 12 muscle related samples, 3 of each type
muscleFiles <- c("Hsmm_", "Hsmmt_", "Hsmmfshd_","Hsmmtubefshd_")
muscleFiles <- paste0(pathToData, muscleFiles, rep(1:3, each=4), ".bed")
RRBSFiles <- c(ewingFiles, muscleFiles)
regionSetFiles <- paste0(pathToData, c("sknmc_specific.bed", "muscle_specific.bed"))

# Let's read the files into R:

BSDTList <- lapply(X=RRBSFiles, FUN=BSreadBiSeq)
regionSets <- lapply(X=regionSetFiles, FUN=fread)
