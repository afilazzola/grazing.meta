data <- read.csv("C:\\Data\\PostDoc\\grazing.meta\\data\\MSdataset.csv", stringsAsFactors=FALSE)

## convert authors to and instead of comma
data$Authors <- as.character(data$Authors)
data$Authors <- gsub(";", " and", data$Authors)

test <- paste0("@article{", data$uniqueID, ",", "\n",
"title = ", "'", data$Title, "'", ",", "\n",
"journal = ", "'", data$Source.Title,"'", ",", "\n",
"volume = ", "'",data$Volume,"'", ",","\n",
"number = ", "'",data$Issue, "'",",", "\n",
"pages = ", "'",paste(data$Beginning.Page, data$Ending.Page, sep="-"),"'",",", "\n",
"year = ", "'",data$Publication.Year,"'", ",", "\n",
"doi = ", "'",data$DOI,"'", ",", "\n",
"author = ", "'",data$Authors,"'", "\n", 
"}", "\n")

write.table(test, "test.txt", row.names=FALSE)