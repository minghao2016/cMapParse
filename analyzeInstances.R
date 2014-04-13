# This script can be used for basic analysis of a cmap text data file
# Read in the merged cell arrays for "anxiety" - five medications, 17 cell lines
# meta should be a subset of the cmap data file (to include the same instances as in the data file)

data = read.table(paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/expression/",filename,"_gedata_norm.txt",sep=""))
meta = read.table(paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/expression/",filename,"_meta.txt",sep=""),head=TRUE)

# Let's look at the distribution for each cell line
par(mfrow=c(4,5))

for (r in 1:dim(data)[2]) {
  hist(data[,r],col="blue",main=meta$cmap_name[r])
}

# Now let's try clusertering based on expression values
tmp = t(data)
labels = as.character(meta$cmap_name)
disty = dist(tmp)
hc = hclust(disty)
par(mfrow=c(1,1))
plot(hc,labels=labels,main="Clustering of Anxiety Medication Cell Lines (by probe expression)")

# Try heatmap
heatmap(tmp,labRow = labels,main="Clustering of Anxiety Medication Cell Lines (by probe expression)")