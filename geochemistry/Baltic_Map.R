require(maps)
require(mapdata)

pdf(file="~/Documents/IODP-347/Metagenomes/Manuscript/Baltic_map.pdf", height=3.5, width=4)
coords <- read.table("~/Documents/IODP-347/site_coordinates.tdt", sep="\t", header=TRUE)
map('worldHires',xlim=c(8,19.2),ylim=c(54,59),mar=c(0,0,0,0))
points(coords$x,coords$y,pch=16,col="red")
text(coords$x,coords$y,coords$site,pos=4)
dev.off()