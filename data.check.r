## load packages
require(reshape2)
require(metafor)
require(tidyverse)

## load dataset
meta <- read.csv("data//binary.simplified.csv")

meta2 <- meta
est <- read.csv("data//Unique.Estimates.Column.csv")
meta2 <- merge(meta2, est, by="Estimate")

## select variable
## join manuscript data
ms.data <- read.csv("data//synthesisdata//manuscript.data.csv")
meta2 <- merge(meta2, ms.data, by="uniqueID")
## subset for abundance only
meta2 <- meta2 %>% filter(est.reduced == "abundance")

## Select domestic grazers only
meta2 <- meta2 %>% filter(grazer.simplified == "domestic")


## Create Unique identifier column
meta2[,"UniqueSite"] <- paste(meta2$uniqueID, meta2$Higher.Taxa, meta2$Taxa.1, meta2$Functional.Group, meta2$estimate.simplified, sep="-")

## convert se to sd
meta2[meta2$Stat=="se","Value"] <- meta2[meta2$Stat=="se","Value"] * 1.96
meta2[meta2$Stat=="se","Stat"] <- "sd"

## drop comparisons that are not pairwise
meta2 <-  meta2 %>% filter(grazing.reduced == "ungrazed" | grazing.reduced == "grazed")
meta2 <- meta2 %>% filter(uniqueID != "30") ## drop study 30 because anomalous 

#meta2 <- meta2 %>% filter(!uniqueID %in% c("159", "654")) ## native grazer not ungulate

meta.data <- meta2
comparison <- meta2$grazing.reduced
site <- meta2$UniqueSite

## determine the number of unique comparisons
ex1 <- meta.data %>% group_by(!!site) %>% summarize(compare.true=length(unique(!!comparison))) %>% filter(compare.true==2) %>% data.frame(.)
colnames(ex1) <- c("UniqueSite","compare.true")

siteID <- matrix(unlist(strsplit(ex1$UniqueSite,"-")),ncol=5, byrow=TRUE)
siteID <- data.frame(siteID) ## recreate as dataframe
colnames(siteID) <- c("Study","higher.taxa","taxa","FG","measure") ## add column names

## The studies used in the comparison
studies <- unique(siteID$Study)
studies <- as.numeric(levels(studies))[studies]
studies <- data.frame(uniqueID=studies)

## load MS data
msdata <- read.csv("data//synthesisdata//manuscript.data.csv")

msvar <- msdata[,c("uniqueID","grazer.spp")]
studies2 <- merge(studies, msvar, by.x="uniqueID", all.x=TRUE)

studies2[c(9,22),"uniqueID"]