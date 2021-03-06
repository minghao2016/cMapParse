# cMapParse: Script 2
# This script will find instances based on drug names, and then output .gct files and .cls files
# to be used for GSEA.  It is recommended when running through the unique cmap_names to do these steps
# manually, as if there is a CEL file from a different platform, it can't read them all in
# together, and you will manually need to alter IDX to only include those from the same array ID.

# VSochat April 2014

# Now we will find instances for drugs, and do analysis

require(gdata)
setwd('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/filtered')
outdir = "/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/expression/anxiety"

# Read in file with instance information
instance_path = ('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances')
instances = read.xls('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/cmap_instances_02.xls',sheet=1)

# Read in file with drug names
meds = read.table('/home/vanessa/Documents/Dropbox/Code/R/connectivityMap/anxietyMeds.dat')
meds = meds$meds
instances$cmap_name = tolower(instances$cmap_name)

# For each medication, find the instance file
found_instances = c()
for (m in 1:length(meds)){
  med = tolower(as.character(meds[m]))
  # Look for instance name
  idx = grep(paste("*",med,"*",sep=""),instances$cmap_name)
  found_instances = c(found_instances,idx)
}

found_instances = instances[found_instances,]

# Let's write our found instances to file
write.table(found_instances,'anxietyInstances.dat',row.names=FALSE)

library('oligo')
library('R.utils')
# For each found instance,

celfiles = c() # Each cel file has multiple controls
controls = list()  # So we keep them in a list
for (i in 1:length(found_instances$perturbation_scan_id)) {
  # This is the instance (cell exposed to drug) CEL file
  id = as.character(found_instances$perturbation_scan_id[i])
  id = gsub("'","",id)
  # This is the control cell (not exposed to drug)
  cid = as.character(found_instances$vehicle_scan_id4[i])
  cid = strsplit(cid,"[.]")
  cid = cid[[1]][-which(cid[[1]] %in% "")]
  # First unzip main data file
  if (!file.exists(paste('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances/',id,".CEL",sep=""))) {
    file = paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances/",id,".CEL.bz2",sep="")
    # Unzip file  
    bunzip2(file) 
  } 
  file = paste('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances/',id,".CEL",sep="")
  celfiles = c(celfiles,file) 
  
  # Now unzip control files
  id = strsplit(id,"[.]")[[1]][1]
  tmp = c()
  for (c in 1:length(cid)){
    cinstance = paste(id,".",cid[c],sep="")
    if (!file.exists(paste('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances/',cinstance,".CEL",sep=""))) {
      file = paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances/",cinstance,".CEL.bz2",sep="")
      # Unzip file  
      bunzip2(file) 
    } 
    file = paste('/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/instances/',cinstance,".CEL",sep="")
    tmp = c(tmp,file)    
  }
  controls = c(controls,list(tmp)) 
}

found_instances = cbind(found_instances,celfiles)

# RUN THIS PART MANUALLY
for (c in 1:length(unique(found_instances$cmap_name))) {
  cmap_name = as.character(unique(found_instances$cmap_name)[c])
  idx = which(found_instances$cmap_name == cmap_name)
  start = idx[1] -1
  con = c()
  # This index needs to be the length of index minus the indices that are different array - DO THIS MANUALLY
  for (i in c(1,2,3,4)) {
    con = c(con,controls[[start+i]])
  }
  labels = c(rep(1,length(celfiles[idx])),rep(2,length(con)))
  combined_data = c(celfiles[idx],con)
  # Let's look at meta info for each
  affySet <- read.celfiles(combined_data)
  affySet = rma(affySet)
  # Write data to output file - will be log2 transformed and normalized
  write.exprs(affySet,file=paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/expression/",cmap_name,"_gedata_norm.txt",sep=""))  
  # Create GSEA labels file
  classfile = paste(outdir,cmap_name,".cls",sep="")
  cat(length(combined_data),"1 2\n",file=classfile)
  cat("# DRUG CON\n",file=classfile,append=TRUE)
  cat(labels,"\n",file=classfile,append=TRUE)  
}

# When we get here, we have raw data in text files, and we need to filter for GSEA
# (use filterDrugGenes.R)

# Let's look at subset that come from the same microarray chip
subby = seq(1,dim(found_instances)[1])
subby = subby[-c(5,13,14)]
affySet <- read.celfiles(celfiles[subby])
affySet = rma(affySet)
# Write data to output file - will be log2 transformed and normalized
filename = paste(as.character(unique(found_instances$cmap_name[subby])),collapse="_")
write.exprs(affySet,file=paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/expression/",filename,"_gedata_norm.txt",sep=""))  
# Save meta info
write.table(meta,file=paste("/home/vanessa/Documents/Work/GENE_EXPRESSION/cmap/expression/",filename,"_meta.txt",sep=""),row.names=FALSE)

