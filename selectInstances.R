# cMapParse: Script 1
# This script will select a subset of instances we are interested in based on a search term -
# For example, for the term anxiety we want the instances related to anxiety medications
# This script uses the DAILY-MED database

# http://dailymed.nlm.nih.gov/

# The bottom portion incorporates RxNorm, although I didn't actually need it!

require(gdata)
require(sqldf)

# Vanessa Sochat April 2014

daily-med = "/home/vanessa/Documents/Work/DRUG/DAILY-MED/"

# Read in files
tmp = read.csv(paste(daily-med,"data/77/DTS_BIG_CON_PROP_ARCH.full",sep=""),sep="\t",quote="",head=TRUE)
role = read.csv(paste(daily-med,"data/77/DTS_COMPLETE_ROLE_CON_ARCHIVE.full",sep=""),sep="\t",quote="",head=TRUE)
concept = read.csv(paste(daily-med,"data/77/DTS_CONCEPT_ARCHIVE.full",sep=""),sep="\t",quote="",head=TRUE)
prop = read.csv(paste(daily-med,"data/77/DTS_INDEXABLE_CON_PROP_ARCHIVE.full",sep=""),sep="\t",quote="",head=TRUE)
ns1 = read.csv(paste(daily-med,"data/77/DTS_SEARCHABLE_CON_PROP_ARCH.full",sep=""),sep="\t",quote="",head=TRUE)

# Save a list of descriptions and gids
gids=c()
desc=c()

# Find which medications are for anxiety - let's search for anxiety and then save the gid numbers
idx = which(apply(tmp, 1, function(x) any(grepl("*anxiety*", x))))
gids = tmp$CONCEPT_GID.number.[idx]         
desc = as.character(tmp$VALUE.blob.[idx])
#idx = which(apply(prop, 1, function(x) any(grepl("*ANXIETY*", x))))
#gids = c(gids,prop$PROPERTY_GID.number.[idx])
#desc = c(desc,as.character(prop$VALUE.string.[idx]))
ns1 = ns1[which(ns1$CONCEPT_GID.number. %in% gids),]
meds = unique(ns1$VALUE.string.)
cat(as.character(meds))

df = as.data.frame(list(meds=meds))
write.table(df,file="anxietyMeds.dat",row.names=FALSE)
cat(as.character(meds),sep="\n")



# RXNORM WORK IS BELOW - WE DON'T ACTUALLY NEED IT
library(RCurl)
library(XML)

# We will save a final list of medications
rxNorm = c()

# Now we need to look up medication names in RxNorm:
for (t in 1:length(meds)){
  med = tolower(as.character(meds[t]))
  xml = getURL(paste('http://rxnav.nlm.nih.gov/REST/approximateTerm?term=',med,"&maxEntries=20&option=1",sep=""))
  xml = xmlParse(xml)
  xml = xmlToList(xml)
  if (length(xml$approximateGroup$candidate) == 0){
    cat("No result for",xml$approximateGroup$inputTerm,"\n")
  }
  # Look up each candidate
  for (c in 1:length(xml$approximateGroup$candidate)){
    candy = xml$approximateGroup$candidate[c]
    if (!is.null(candy)) {
      xml = getURL(paste("http://rxnav.nlm.nih.gov/REST/rxcui/",candy$rxcui,"/allrelated",sep=""))
      xml = xmlParse(xml)
      xml = xmlToList(xml)
      for (r in 2:length(xml$allRelatedGroup$conceptGroup)){
        rxNorm = c(rxNorm,xml$allRelatedGroup$conceptGroup[r]$conceptProperties$name)        
      }
     }
  }
}

# Final list of medications is in RxNorm
# General labels (what we need) are in "meds"
cat(as.character(meds))
