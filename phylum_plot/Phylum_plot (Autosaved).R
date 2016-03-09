setwd("/Users/ianmarshall/Documents/IODP-347/Metagenomes/Manuscript/baltic-metagenomes/phylum_plot/")

phylum.counts <-read.table("CREST_phylum_counts",header=TRUE,sep="\t")

phylum.counts.notaxonpath <- data.frame(phylum.counts[,4:16],row.names=phylum.counts$Taxon)

sample.sums <- vector()

combined.60B.9 <- phylum.counts[,"X60B.9.replicate"] + phylum.counts[,"X60B.9"]
combined.63E.6 <- phylum.counts[,"X63E.6.A"] + phylum.counts[,"X63E.6.B"]
combined.65C.4 <- phylum.counts[,"X65C.4"] + phylum.counts[,"X65C.4.replicate"]

phylum.counts.notaxonpath.nosampleswithreps <- phylum.counts.notaxonpath[,c(1,2,3,6,7,10,13)]

phylum.counts.notaxonpath.nosampleswithreps$X60B.9 <- combined.60B.9
phylum.counts.notaxonpath.nosampleswithreps$X63E.6 <- combined.63E.6
phylum.counts.notaxonpath.nosampleswithreps$X65C.4 <- combined.65C.4

phylum.counts.notaxonpath.noreps <- phylum.counts.notaxonpath.nosampleswithreps[,c(1,2,3,8,4,5,9,6,10,7)]

for(i in 1:dim(phylum.counts.notaxonpath.noreps)[2])
{
	sample.sums[length(sample.sums)+1] <- sum(phylum.counts.notaxonpath.noreps[,i])
}






phylum.percent <- data.frame()

for(i in 1:dim(phylum.counts.notaxonpath.noreps)[1])
{

percent.row <- phylum.counts.notaxonpath.noreps[i,]/sample.sums


phylum.percent <- rbind(phylum.percent,percent.row)
}

library(made4)



barplot(as.matrix(phylum.percent), axes=FALSE, col= rainbow_hcl(dim(phylum.counts.notaxonpath)[1]))


phylum.sums <- vector()

for(i in 1:dim(phylum.counts.notaxonpath.noreps)[1])
{
	phylum.sums[length(phylum.sums)+1] <- sum(phylum.percent[i,])
}

barplot(phylum.sums,names.arg=row.names(phylum.percent),horiz=TRUE)


other.phyla <- vector(length=dim(phylum.percent)[2])

phylum.percent.others <- data.frame(row.names=row.names(phylum.percent))

for(i in 1:dim(phylum.percent)[2])
{
	new.column <- vector()
	other.phyla[i] = 0
	for(j in 1:dim(phylum.percent)[1])
	{
		if(phylum.percent[j,i]<0.05)
		{
			other.phyla[i] = other.phyla[i] + phylum.percent[j,i]
			new.column[j] = 0
		}
		else
		{
			new.column[j] = phylum.percent[j,i]
		}
	}
	phylum.percent.others <- cbind(phylum.percent.others,new.column)
}

new.row.names <- row.names(phylum.percent.others)
new.row.names[length(new.row.names)+1] <- "Other"

phylum.percent.others <- rbind(phylum.percent.others,other.phyla)

row.names(phylum.percent.others) <- new.row.names

phylum.percent.trimmed <- data.frame()

for(i in 1:dim(phylum.percent.others)[1])
{
	if(sum(phylum.percent.others[i,]) > 0)
	{
		phylum.percent.trimmed <- rbind(phylum.percent.trimmed,phylum.percent.others[i,])
	}
}

sample_names = c("59E-13","59E-21","59E-25","60B-9","60B-13","60B-28","63E-6","63E-24","65C-4","65C-10")

color_vector = sample(rainbow(17))

pdf(file="../../phylum_plot_V5.pdf",width=18,height=7)

barplot(as.matrix(phylum.percent.trimmed), col= color_vector, names=sample_names)

colour_by_numbers = rev(1:17)

for(i in 1:dim(as.matrix(phylum.percent.trimmed))[2])
{
	height=0
	for(j in 1:dim(as.matrix(phylum.percent.trimmed))[1])
	{
		if(as.matrix(phylum.percent.trimmed)[j,i] > 0)
		{
			print(i)
			height = height + as.matrix(phylum.percent.trimmed)[j,i]/2
			text(i+0.2*i-0.5,height, colour_by_numbers[j])
			height = height + as.matrix(phylum.percent.trimmed)[j,i]/2
		}
	}
}

for(i in 1:length(sample.sums))
{
	string <- paste("n=", sample.sums[i],sep="")
	mtext(side=1, text=string, line=2, at=i*1.2-0.5)
}
dev.off()

pdf(file="../../phylum_key_V5.pdf",width=3,height=7)
par(mar=c(0,0,0,0))
plot(c(-10,-10),ylim=c(0,17),xlim=c(0,10),axes=FALSE,ylab="",xlab="")

for(i in 1:dim(phylum.percent.trimmed)[1])
{
	rect(0,i-1,1,i,col= color_vector[i])
	text(0.5,i-0.5,colour_by_numbers[i])
	text(1,i-0.5,row.names(phylum.percent.trimmed)[i],pos=4)
}
dev.off()
