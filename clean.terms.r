## clean up master data file for binary comparisons

library(tidyverse)

data <- read.csv("data//binary.data.csv")

str(data)

## Unique stat values
unique(data$Stat)

data[,"Stat"] <- gsub("sterror", "se", data$Stat) ## standardize error terms
data[,"Stat"] <- gsub("sttrror", "se", data$Stat) ## standardize error terms
data[,"Stat"] <- gsub("stdev", "sd", data$Stat) ## standardize SD terms
data[,"Stat"] <- gsub("Mean", "mean", data$Stat) ## standardize mean terms
data[,"Stat"] <- gsub("mean ", "mean", data$Stat) ## standardize mean terms
data[,"Stat"] <- gsub("Stdev", "sd", data$Stat) ## standardize sd terms
data[,"Stat"] <- gsub("SE", "se", data$Stat) ## standardize mean terms
data[,"Stat"] <- gsub("adjusted mean", "mean", data$Stat) ## standardize mean terms
data[,"Stat"] <- gsub("standard error", "se", data$Stat) ## standardize error terms
data[,"Stat"] <- gsub("Count", "count", data$Stat) ## standardize count terms
data[,"Stat"] <- gsub("SD", "sd", data$Stat) ## standardize SD terms
data[,"Stat"] <- gsub("DS", "sd", data$Stat) ## standardize SD terms
data[,"Stat"] <- gsub("Standard Deviation", "sd", data$Stat) ## standardize SD terms

## Convert LSmean to mean
data[,"Stat"] <- gsub("LSmean", "mean", data$Stat) ## standardize mean terms

## Convert index to count
data[,"Stat"] <- gsub("Index", "count", data$Stat) ## standardize mean terms

## Convert estimate to mean
data[,"Stat"] <- gsub("estimate", "mean", data$Stat) ## standardize mean terms


## manually calculated SE from papers that had CI limits
## remove CI and quartile datasets

data <- data[!data$Stat=="Lower 95% CI",] ## remove lower CI
data <- data[!data$Stat=="Upper 95% CI",] ## remove Upper CI
data <- data[!data$Stat=="lower 95% CI",] ## remove lower CI
data <- data[!data$Stat=="upper 95% CI",] ## remove Upper CI
data <- data[!data$Stat=="1st quartile",] ## 1st quartile
data <- data[!data$Stat=="3rd quartile",] ## 3rd quartile
data <- data[!data$Stat=="higher 95% CI",] ## upper CI

## Convert CIs to se
data[data$Stat=="95% ci","Value"] <- data[data$Stat=="95% ci","Value"]/1.96 ## convert CI to se
data[data$Stat=="95% ci","Stat"] <- "se" ## relabel as se
data[data$Stat=="95% Cis","Value"] <- data[data$Stat=="95% Cis","Value"]/1.96 ## convert CI to se
data[data$Stat=="95% Cis","Stat"] <- "se" ## relabel as se

## Convert 2 SE to se
data[data$Stat=="2 se","Value"] <- data[data$Stat=="2 se","Value"]/2 ## convert 2 se to se
data[data$Stat=="2 se","Stat"] <- "se" ## relabel as se


## Compare estimates
unique(data$Estimate)

## write.csv(unique(data$Estimate), "Unique.Estimates.Column.csv")

## Read in Functional Group summary column
fg <- read.csv("functional.groups.csv")


data.fg <- merge(data, fg, by=c("uniqueID","Taxa","Genus","Species","Estimate"))

write.csv(data.fg, "data//binary.simplified.csv")

