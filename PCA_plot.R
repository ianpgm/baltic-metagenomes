setwd("~/Documents/IODP-347/Metagenomes/Manuscript/")

Baltic_rpoB_eq_noreps = read.table("Baltic_rpoB_eq_noreps", sep="\t", header=TRUE)

Baltic_rpoB_eq_noreps_rowsamples = t(Baltic_rpoB_eq_noreps)

library(vegan)

PCA <- rda(Baltic_rpoB_eq_noreps_rowsamples)

pdf(file="PCA.pdf", width=7, height=7)
biplot(PCA, display="sites")
dev.off()