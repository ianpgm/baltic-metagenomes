setwd("~/Documents/IODP-347/Metagenomes/Manuscript/")

pdf(file="Geochem_plot.pdf", width=10, height=6)
par(mfcol=c(1,4),cex.lab=1.4, cex.axis=1.15)


#Site 59E
TOC.59E <- read.table("347-M0059E_TS_TC_TOC_raw.tab", skip=23, sep="\t", header=TRUE)
Salinity.59E <- read.table("347-M0059E_water_chem.tab",skip=22, sep="\t", header=TRUE, nrows=33)
Sulfate.59 <- read.table("347-M0059_IW.tab",skip=45,sep="\t",header=TRUE)
Sulfate.59E <- Sulfate.59[Sulfate.59$Event == "347-M0059E",][Sulfate.59[Sulfate.59$Event == "347-M0059E",]$Type == "Squeezed sample",]

par(mar=c(6, 4, 5, 0) + 0.1)
plot(TOC.59E$TOC....,TOC.59E$Depth..m.,ylim=c(85,0), ylab="Depth (mbsf)", xlim=c(0,7),xlab="", pch=20)
points(Salinity.59E$Sal..Cl.based./4,Salinity.59E$Depth..m.,col="red", pch=20)
points(Sulfate.59E$X.SO4.2...mmol.l.*7,Sulfate.59E$Depth..m.,col="blue", pch=20)


axis(side=3, at=0:7,labels=c(0,4,8,12,16,20,24,28),col="red",col.axis="red",)
axis(side=1, at=(0:10)*7/10,labels=round((0:10)*(0.1),digits=2),col="blue",col.axis="blue",line=3)

mtext("Salinity", side = 3, line = 2,col="red", cex=0.8)
mtext("TOC %", side = 1, line = 2,col="black", cex=0.8)
mtext("Sulfate (mM)",side=1,line=5,col="blue", cex=0.8)
mtext("M0059",side=3,line=3.5, font=1, cex=1.2)

sample.depths.59E = c(40.95,67.5,80.55)
sample.names.59E = c("59E-13","59E-21","59E-25")

for(i in 1:3)
{
	lines(c(-1,10),c(sample.depths.59E[i],sample.depths.59E[i]), col="red", lwd=3)
	text(2,sample.depths.59E[i],labels=sample.names.59E[i], pos=3, col="red")
}

#lines(c(-1,10),c(52,52),col="blue",lwd=3)
#text(2,52,labels="Holocene", pos=3, col="blue")
#text(2,52,labels="Glacial", pos=1, col="blue")



#Site 60B
TOC.60A <- read.table("347-M0060A_TS_TC_TOC_raw.tab", skip=24, sep="\t", header=TRUE)
Salinity.60B <-read.table("347-M0060B_water_chem.tab",skip=21,sep="\t",header=TRUE,nrows=28)
Sulfate.60 <-read.table("347-M0060_IW.tab",skip=41,sep="\t",header=TRUE)
Sulfate.60B <- Sulfate.60[Sulfate.60$Event == "347-M0060B",][Sulfate.60[Sulfate.60$Event == "347-M0060B",]$Type == "Squeezed sample",]

par(mar=c(6, 0, 5, 0) + 0.1)
plot(TOC.60A$TOC....,TOC.60A$Depth..m.,ylim=c(85,0),xlab="", ,xlim=c(0,7),yaxt="n",ylab="", pch=20)
points(Salinity.60B$Sal/5,Salinity.60B$Depth..m., col="red", pch=20)
points(Sulfate.60B$X.SO4.2...mmol.l./4,Sulfate.60B$Depth..m., col="blue", pch=20)

axis(side=3, at=0:7,labels=c(0:7)*5,col="red",col.axis="red",)
axis(side=1, at=0:7,labels=0:7*4,col="blue",col.axis="blue",line=3)

mtext("Salinity", side = 3, line = 2,col="red", cex=0.8)
mtext("TOC %", side = 1, line = 2,col="black", cex=0.8)
mtext("Sulfate (mM)",side=1,line=5,col="blue", cex=0.8)
mtext("M0060",side=3,line=3.5, font=1, cex=1.2)

sample.depths.60B = c(27.1,37.3,84.4)
sample.names.60B = c("60B-9","60B-13","60B-28")

for(i in 1:3)
{
	lines(c(-1,10),c(sample.depths.60B[i],sample.depths.60B[i]), col="red", lwd=3)
	text(2,sample.depths.60B[i],labels=sample.names.60B[i], pos=3, col="red")
}

#Site 63E
TOC.63E <- read.table("347-M0063E_TS_TC_TOC_raw.tab", skip=23, sep="\t", header=TRUE)
Salinity.63E <- read.table("347-M0063E_water_chem.tab",skip=21,sep="\t",header=TRUE,nrows=51)
Sulfate.63 <- read.table("347-M0063_IW.tab",skip=46,sep="\t",header=TRUE)
Sulfate.63E <- Sulfate.63[Sulfate.63$Event == "347-M0063E",][Sulfate.63[Sulfate.63$Event == "347-M0063E",]$Type == "Squeezed sample",]


plot(TOC.63E$TOC....,TOC.63E$Depth..m.,ylim=c(85,0),xlab="", xlim=c(0,7),yaxt="n",ylab="", pch=20)
points(Salinity.63E$Sal/4,Salinity.63E$Depth..m.,col="red", pch=20)
points(Sulfate.63E$X.SO4.2...mmol.l.*3,Sulfate.63E$Depth..m.,col="blue", pch=20)

axis(side=3, at=0:7,labels=c(0:7)*4,col="red",col.axis="red",)
axis(side=1, at=0:10*7/10,labels=0:10*0.2,col="blue",col.axis="blue",line=3)

mtext("Salinity", side = 3, line = 2,col="red", cex=0.8)
mtext("TOC %", side = 1, line = 2,col="black", cex=0.8)
mtext("Sulfate (mM)",side=1,line=5,col="blue", cex=0.8)
mtext("M0063",side=3,line=3.5, font=1, cex=1.2)



sample.depths.63E = c(11.35,47.35)
sample.names.63E = c("63E-6","63E-24")

for(i in 1:2)
{
	lines(c(-1,10),c(sample.depths.63E[i],sample.depths.63E[i]), col="red", lwd=3)
	text(2,sample.depths.63E[i],labels=sample.names.63E[i], pos=3, col="red")
}

#lines(c(-1,10),c(34,34),col="blue",lwd=3)
#text(2,34,labels="Holocene", pos=3, col="blue")
#text(2,34,labels="Glacial", pos=1, col="blue")


#Site 65C
TOC.65C <- read.table("347-M0065C_TS_TC_TOC_raw.tab", skip=23, sep="\t", header=TRUE)
Salinity.65C <- read.table("347-M0065C_water_chem.tab", skip=21,sep="\t",header=TRUE, nrows=20)
Sulfate.65 <- read.table("347-M0065_IW.tab",skip=42,sep="\t",header=TRUE)
Sulfate.65C <- Sulfate.65[Sulfate.65$Event == "347-M0065C",][Sulfate.65[Sulfate.65$Event == "347-M0065C",]$Type == "Squeezed sample",]


plot(TOC.65C$TOC....,TOC.65C$Depth..m.,ylim=c(85,0),xlab="", xlim=c(0,7),yaxt="n",ylab="", pch=20)
points(Salinity.65C$Sal/4,Salinity.65C$Depth..m.,col="red", pch=20)
points(Sulfate.65C$X.SO4.2...mmol.l.*3,Sulfate.65C$Depth..m.,col="blue", pch=20)

axis(side=3, at=0:7,labels=c(0:7)*4,col="red",col.axis="red",)
axis(side=1, at=0:10*7/10,labels=0:10*0.2,col="blue",col.axis="blue",line=3)

mtext("Salinity", side = 3, line = 2,col="red", cex=0.8)
mtext("TOC %", side = 1, line = 2,col="black", cex=0.8)
mtext("Sulfate (mM)",side=1,line=5,col="blue", cex=0.8)
mtext("M0065",side=3,line=3.5, font=1, cex=1.2)


sample.depths.65C = c(10.10,29.90)
sample.names.65C = c("65C-4","65C-10")

for(i in 1:2)
{
	lines(c(-1,10),c(sample.depths.65C[i],sample.depths.65C[i]), col="red", lwd=3)
	text(2,sample.depths.65C[i],labels=sample.names.65C[i], pos=3, col="red")
}

#lines(c(-1,10),c(13,13),col="blue",lwd=3)
#text(2,13,labels="Holocene", pos=3, col="blue")
#text(2,13,labels="Glacial", pos=1, col="blue")
dev.off()